import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings
import com.github.vlsi.gradle.properties.dsl.props

plugins {
    `java-library`
    `module-info-compile`
    id("com.github.vlsi.crlf")
}

dependencies {
    api(projects.darklafTheme)
    api(projects.darklafPropertyLoader)
    api(projects.darklafIconset)
    api(projects.darklafUtils)
    implementation(projects.darklafCompatibility)
    implementation(projects.darklafNativeUtils)
    implementation(projects.darklafPlatformBase)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
    implementation(libs.swingDslLafSupport)
    implementation(libs.jsvg)

    compileOnly(libs.nullabilityAnnotations)
    compileOnly(libs.swingx)
    compileOnly(toolLibs.errorprone.annotations)
    compileOnly(toolLibs.autoservice.annotations)
    annotationProcessor(toolLibs.autoservice.processor)

    testImplementation(libs.jsvg)
    testImplementation(libs.swingx)
    testImplementation(testLibs.bundles.miglayout)
    testImplementation(testLibs.swingDslInspector)
    testImplementation(testLibs.jna)
    testImplementation(testLibs.rsyntaxtextarea)
    testImplementation(testLibs.lGoodDatePicker)
    testImplementation(testLibs.junit.api)
    testRuntimeOnly(testLibs.junit.engine)
    testCompileOnly(libs.nullabilityAnnotations)
}

tasks.processResources {
    into("com/github/weisj/darklaf/external/jdk") {
        from(rootDir.resolve("externalResources/jdk/basic"))
        from(rootDir.resolve("externalResources/jdk/metal"))
    }
}

moduleInfo {
    modularExec {
        patchJUnit = true
        addExports.add("java.desktop/com.sun.java.swing=darklaf.core")
        openTestPackagesTo("darklaf.properties")
    }
}

tasks.test {
    doFirst {
        workingDir = File(project.rootDir, "build/test_results")
        workingDir.mkdirs()
    }
    useJUnitPlatform()
    val verboseTest by props(false)
    if (!verboseTest) {
        exclude("**/DemoTest*")
    }
}

fun Jar.includeLicenses() {
    CrLfSpec(LineEndings.LF).run {
        into("META-INF") {
            filteringCharset = "UTF-8"
            duplicatesStrategy = DuplicatesStrategy.EXCLUDE
            textFrom("$rootDir/licenses/DARCULA_LICENSE.txt")
            textFrom("$rootDir/licenses/INTELLIJ_LICENSE.txt")
            textFrom("$rootDir/licenses/INTELLIJ_NOTICE.txt")
            textFrom("$rootDir/licenses/PBJAR_LICENSE.txt")
        }
    }
}

tasks.jar {
    includeLicenses()
}

val makeDocumentation by tasks.registering(JavaExec::class) {
    group = "Development"
    description = "Builds the documentation"
    dependsOn(tasks.testClasses)

    workingDir = File(project.rootDir, "build").apply { mkdirs() }
    mainClass.set("com.github.weisj.darklaf.core.documentation.CreateUITable")
    classpath(sourceSets.main.get().runtimeClasspath, sourceSets.test.get().runtimeClasspath)
}

abstract class DemoTask : JavaExec() {
    init {
        setMainClass("com.github.weisj.darklaf.ui.DemoLauncher")
    }

    @Option(
        option = "class",
        description = "Specifies the main class to run (e.g. com.github.weisj.darklaf.ui.table.TableDemo, com.github.weisj.ui.button.ButtonDemo, ...)"
    )
    fun setMainClass(mainClassName: String?) = mainClass.set(mainClassName)
}

val runDemo by tasks.registering(DemoTask::class) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    description =
        "Launches demo (e.g. com.github.weisj.darklaf.ui.table.TableDemo, com.github.weisj.darklaf.ui.button.ButtonDemo, ...)"

    dependsOn(tasks.compileJava, tasks.compileTestJava)

    // Pass the property to the demo
    // By default JavaExec is executed in its own JVM with its own properties
    // It allows to pass system properties via gradlew -Ddarklaf.prop=value
    fun passProperty(name: String, default: String? = null) {
        val value = System.getProperty(name) ?: default
        value?.let { systemProperty(name, it) }
    }

    val props = System.getProperties()
    @Suppress("UNCHECKED_CAST")
    for (e in props.propertyNames() as `java.util`.Enumeration<String>) {
        if (e.startsWith("darklaf.")) {
            passProperty(e)
        }
    }
    passProperty("java.awt.headless")
}
