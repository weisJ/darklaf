import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    id("com.github.johnrengelman.shadow")
    id("com.github.vlsi.crlf")
}

dependencies {
    implementation(project(":darklaf-native-utils"))
    implementation(project(":darklaf-utils"))
    implementation(project(":darklaf-windows"))
    implementation(project(":darklaf-property-loader"))
    implementation("org.swinglabs:jxlayer")
    compileOnly("org.swinglabs:swingx")
    testImplementation("com.formdev:svgSalamander")
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
