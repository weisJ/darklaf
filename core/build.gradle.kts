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
    api(projects.darklafUtils)
    implementation(projects.darklafCompatibility)
    implementation(projects.darklafIconset)
    implementation(projects.darklafNativeUtils)
    implementation(projects.darklafPlatformBase)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
    implementation(libs.swingDslLafSupport)
    implementation(libs.svgSalamander)

    compileOnly(libs.nullabilityAnnotations)
    compileOnly(libs.swingx)

    testImplementation(libs.jna)
    testImplementation(libs.svgSalamander)
    testImplementation(libs.bundles.test.miglayout)
    testImplementation(libs.swingx)
    testImplementation(libs.test.rsyntaxtextarea)
    testImplementation(libs.test.lGoodDatePicker)
    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)

    compileOnly(libs.autoservice.annotations)
    annotationProcessor(libs.autoservice.processor)
}

tasks.test {
    doFirst {
        workingDir = File(project.rootDir, "build/test_results")
        workingDir.mkdirs()
    }
    useJUnitPlatform()
    patchTestExecParams()
    val verboseTest by props(true)
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

    workingDir = File(project.rootDir, "build")
    workingDir.mkdirs()
    main = "com.github.weisj.darklaf.core.documentation.CreateUITable"
    classpath(sourceSets.main.get().runtimeClasspath, sourceSets.test.get().runtimeClasspath)
}

abstract class DemoTask : JavaExec() {
    init {
        main = "com.github.weisj.darklaf.ui.DemoLauncher"
    }

    @Option(
        option = "class",
        description = "Specifies the main class to run (e.g. com.github.weisj.darklaf.ui.table.TableDemo, com.github.weisj.ui.button.ButtonDemo, ...)"
    )
    override fun setMain(mainClassName: String?) = super.setMain(mainClassName)
}

val runDemo by tasks.registering(DemoTask::class) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    description =
        "Launches demo (e.g. com.github.weisj.darklaf.ui.table.TableDemo, com.github.weisj.darklaf.ui.button.ButtonDemo, ...)"

    classpath(sourceSets.test.map { it.runtimeClasspath })

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
