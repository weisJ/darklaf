import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    id("com.github.vlsi.crlf")
}

dependencies {
    api(project(":darklaf-theme"))
    api(project(":darklaf-property-loader"))
    api(project(":darklaf-utils"))
    implementation(project(":darklaf-native-utils"))
    implementation(project(":darklaf-platform-base"))
    implementation(project(":darklaf-windows"))
    implementation(project(":darklaf-macos"))
    implementation("org.swinglabs:jxlayer")
    implementation("com.formdev:svgSalamander")
    compileOnly("org.swinglabs:swingx")

    testImplementation("com.formdev:svgSalamander")
    testImplementation("com.miglayout:miglayout-core")
    testImplementation("com.miglayout:miglayout-swing")
    testImplementation("org.swinglabs:swingx")
    testImplementation("org.junit.jupiter:junit-jupiter-api")
    testImplementation("com.fifesoft:rsyntaxtextarea")
    testImplementation("com.github.lgooddatepicker:LGoodDatePicker")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine")

    annotationProcessor("com.google.auto.service:auto-service")
    compileOnly("com.google.auto.service:auto-service-annotations")
}

tasks.test {
    useJUnitPlatform()
    workingDir = File(project.rootDir, "build/test_results")
    workingDir.mkdirs()
}

fun Jar.includeLicenses() {
    CrLfSpec(LineEndings.LF).run {
        into("META-INF") {
            filteringCharset = "UTF-8"
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
    main = "documentation.CreateUITable"
    classpath(sourceSets.main.get().runtimeClasspath, sourceSets.test.get().runtimeClasspath)
}

abstract class DemoTask : JavaExec() {
    init {
        main = "DemoLauncher"
    }

    @Option(
        option = "class",
        description = "Specifies the main class to run (e.g. UIDemo, ui.table.TableDemo, ui.button.ButtonDemo, ...)"
    )
    override fun setMain(mainClassName: String?) = super.setMain(mainClassName)
}

val runDemo by tasks.registering(DemoTask::class) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    description = "Launches demo (e.g. DemoLauncher, ui.table.TableDemo, ui.button.ButtonDemo, ...)"

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
