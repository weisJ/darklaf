import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    id("com.github.johnrengelman.shadow")
    id("com.github.vlsi.crlf")
}

dependencies {
    implementation(project(":darklaf-theme"))
    implementation(project(":darklaf-native-utils"))
    implementation(project(":darklaf-utils"))
    implementation(project(":darklaf-platform-base"))
    implementation(project(":darklaf-windows"))
    implementation(project(":darklaf-macos"))
    implementation(project(":darklaf-property-loader"))
    implementation("org.swinglabs:jxlayer")
    compileOnly("org.swinglabs:swingx")
    testImplementation("com.formdev:svgSalamander")
    testImplementation("com.miglayout:miglayout-core")
    testImplementation("com.miglayout:miglayout-swing")
    testImplementation("org.swinglabs:swingx")
}

tasks.jar {
    CrLfSpec(LineEndings.LF).run {
        into("META-INF") {
            filteringCharset = "UTF-8"
            textFrom("licenses/NOTICE.txt")
            textFrom("licenses/DARCULA_LICENSE.txt")
            textFrom("licenses/PBJAR_LICENSE.txt")
            textFrom("licenses/INTELLIJ_LICENSE.txt")
        }
    }
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

tasks.shadowJar {
    exclude("help/")
    exclude("icons/")
    exclude("org/jdesktop/jxlayer/plaf/ext/images/")
    exclude("com/sun/jna/darwin/")
    exclude("com/sun/jna/freebsd-x86/")
    exclude("com/sun/jna/freebsd-x86-64/")
    exclude("com/sun/jna/linux-arm/")
    exclude("com/sun/jna/linux-x86/")
    exclude("com/sun/jna/linux-x86-64/")
    exclude("com/sun/jna/openbsd-x86/")
    exclude("com/sun/jna/openbsd-x86-64/")
    exclude("com/sun/jna/sunos-sparc/")
    exclude("com/sun/jna/sunos-sparcv9/")
    exclude("com/sun/jna/sunos-x86/")
    exclude("com/sun/jna/sunos-x86-64/")
}

abstract class DemoTask : JavaExec() {
    init {
        setMain("UIDemo")
    }

    @Option(option = "class", description = "Specifies the main class to run (e.g. UIDemo, ui.table.TableDemo, ui.button.ButtonDemo, ...)")
    override fun setMain(mainClassName: String?) = super.setMain(mainClassName)
}

val runDemo by tasks.registering(DemoTask::class) {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    description = "Launches demo (e.g. UIDemo, ui.table.TableDemo, ui.button.ButtonDemo, ...)"

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
