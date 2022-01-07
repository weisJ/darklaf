import org.gradle.api.JavaVersion
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.tasks.Copy
import org.gradle.api.tasks.JavaExec
import org.gradle.api.tasks.SourceSet
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.api.tasks.compile.JavaCompile
import org.gradle.kotlin.dsl.getValue
import org.gradle.kotlin.dsl.named
import org.gradle.kotlin.dsl.provideDelegate
import org.gradle.kotlin.dsl.registering
import org.gradle.kotlin.dsl.withType
import org.gradle.process.JavaForkOptions
import java.util.regex.Pattern

class ExecParameters(
    var addExports : MutableList<String> = mutableListOf(),
    var addReads : MutableList<String> = mutableListOf(),
    var addOpens : MutableList<String> = mutableListOf(),
    var patchJUnit : Boolean = false,
) {
    internal val testPackagesOpens : MutableList<String> = mutableListOf()

    fun openTestPackagesTo(vararg modules : String) {
        testPackagesOpens.addAll(modules)
    }
}

open class ModuleInfoExtension {
    var version: JavaVersion = JavaVersion.VERSION_1_9
    var extraArgs: List<String> = emptyList()
    lateinit var moduleName: String
    val execParameters = ExecParameters()

    fun modularExec(action : ExecParameters.() -> Unit) = execParameters.action()
}

class ModuleInfoCompilePlugin : Plugin<Project> {

    companion object {
        private val MODULE_PATTERN : Pattern = Pattern.compile("""module\s+([^\s]+)\s*\{""")
    }

    override fun apply(target: Project) = target.run {
        val infoExtension = target.extensions.create("moduleInfo", ModuleInfoExtension::class.java)
        if (!JavaVersion.current().isJava9Compatible
            || project.findProperty("skipModuleInfo") in listOf("", "true")) return@run

        val moduleInfoFile = file("src/main/module/module-info.java")
        if (moduleInfoFile.exists()) {
            val moduleNameMatcher = MODULE_PATTERN.matcher(moduleInfoFile.readText())
            moduleNameMatcher.find()
            infoExtension.moduleName = moduleNameMatcher.group(1)

            println("${project.name} ==> ${infoExtension.moduleName}")

            setupModuleInfoCompilation(infoExtension)

            tasks.withType<JavaExec>().configureEach {
                doFirst {
                    patchTestExecParams(target, infoExtension)
                }
            }
        }
    }

    private fun Project.setupModuleInfoCompilation(infoExtension : ModuleInfoExtension) {
        val compileJava = tasks.named<JavaCompile>("compileJava")

        val compileModuleInfoJava by tasks.registering(JavaCompile::class) {
            val javaCompile = compileJava.get()
            classpath = files()
            source("src/main/module/module-info.java")
            source(javaCompile.source)
            destinationDirectory.set(buildDir.resolve("classes/module"))
            check(infoExtension.version.isJava9Compatible)
            options.compilerArgs.addAll(listOf("--module-path", javaCompile.classpath.asPath))
            if (infoExtension.extraArgs.isNotEmpty()) {
                options.compilerArgs.addAll(infoExtension.extraArgs)
                sourceCompatibility = infoExtension.version.majorVersion
                targetCompatibility = infoExtension.version.majorVersion
            } else {
                options.compilerArgs.addAll(listOf("--release", infoExtension.version.majorVersion))
            }
        }

        val copyModuleInfo by tasks.registering(Copy::class) {
            dependsOn(compileModuleInfoJava)
            from(buildDir.resolve("classes/module/module-info.class"))
            into(buildDir.resolve("classes/java/main"))
        }

        compileJava.configure {
            dependsOn(copyModuleInfo)
            taskDependencies.getDependencies(this).forEach {
                if (it.project != this@setupModuleInfoCompilation || it.name != "copyModuleInfo") {
                    compileModuleInfoJava.get().dependsOn(it)
                }
            }
        }
    }

    private fun JavaForkOptions.patchTestExecParams(project : Project, infoExtension: ModuleInfoExtension) {
        val sourceSets = project.sourceSets
        val testSourceSet = sourceSets.test
        val mainSourceSet = sourceSets.main
        val patchFiles = testSourceSet.output.classesDirs +
                testSourceSet.resources.sourceDirectories +
                mainSourceSet.resources.sourceDirectories
        val resourceDir = testSourceSet.resources.sourceDirectories.singleFile
        val testPackages = testSourceSet.resources.asSequence().map { it.parentFile }.toSet().asSequence().map {
            it.relativeTo(resourceDir).toPath().joinToString(separator = ".")
        }.filter { it.isNotEmpty() }
        jvmArgs(
            "--module-path", (testSourceSet.runtimeClasspath - patchFiles).asPath,
            "--patch-module", "${infoExtension.moduleName}=${patchFiles.asPath}",
            "--add-modules", "ALL-MODULE-PATH",
            "--add-reads", "${infoExtension.moduleName}=org.junit.jupiter.api"
        )
        if (infoExtension.execParameters.patchJUnit) {
            jvmArgs(
                "--add-reads", "${infoExtension.moduleName}=org.junit.jupiter.api",
                "--add-exports", "org.junit.platform.commons/org.junit.platform.commons.util=ALL-UNNAMED",
                "--add-exports", "org.junit.platform.commons/org.junit.platform.commons.logging=ALL-UNNAMED",
            )
            testPackages.forEach {
                jvmArgs(
                    "--add-opens", "${infoExtension.moduleName}/$it=org.junit.platform.commons"
                )
                infoExtension.execParameters.testPackagesOpens.forEach { module ->
                    jvmArgs("--add-opens", "${infoExtension.moduleName}/$it=$module")
                }
            }
        }
        infoExtension.execParameters.addReads.forEach {
            jvmArgs("--add-reads", it)
        }
        infoExtension.execParameters.addExports.forEach {
            jvmArgs("--add-exports", it)
        }
        infoExtension.execParameters.addOpens.forEach {
            jvmArgs("--add-reads", it)
        }
    }

    private val Project.sourceSets : SourceSetContainer
        get() = extensions.getByName("sourceSets") as SourceSetContainer

    private val SourceSetContainer.test : SourceSet
        get() = named<SourceSet>("test").get()

    private val SourceSetContainer.main : SourceSet
        get() = named<SourceSet>("main").get()
}
