import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    `module-info-compile`
    id("com.github.vlsi.crlf")
}

configure<ModuleInfoExtension> {
    version = JavaVersion.VERSION_11
    extraArgs = listOf(
        "--add-exports", "java.desktop/sun.awt=darklaf.compatibility"
    )
    stubModule("darklaf.core")
}

dependencies {
    implementation(projects.darklafUtils)
}

fun Jar.includeLicenses() {
    CrLfSpec(LineEndings.LF).run {
        into("META-INF") {
            filteringCharset = "UTF-8"
            duplicatesStrategy = DuplicatesStrategy.EXCLUDE
            textFrom("$rootDir/licenses/INTELLIJ_LICENSE.txt")
            textFrom("$rootDir/licenses/INTELLIJ_NOTICE.txt")
        }
    }
}

tasks.jar {
    includeLicenses()
}
