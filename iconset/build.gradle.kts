import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    `module-info-compile`
    id("com.github.vlsi.crlf")
}

dependencies {
    api(projects.darklafPropertyLoader)
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

val generateIconAccessor by tasks.registering {
    doFirst {
        sourceSets.main.configure {
            val propertyFile = project.file("iconAccessorSpec.properties")
            val allIconsName = "AllIcons"
            val generatedDir = project.buildDir.resolve("generated/iconAccessor")
            generatedDir.mkdirs()
            generatedDir.resolve("$allIconsName.java").apply {
                createNewFile()
                writeText(createIconAccessor(propertyFile, allIconsName))
            }
            java.srcDir(generatedDir)
        }
    }
}

tasks.compileJava.configure {
    dependsOn(generateIconAccessor)
}
