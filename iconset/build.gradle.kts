import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
    `module-info-compile`
    id("com.github.vlsi.crlf")
}

dependencies {
    api(projects.darklafPropertyLoader)
    compileOnly(libs.javaxAnnotations)
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
    val generatedDir = project.buildDir.resolve("generated/iconAccessor")
    sourceSets.main.configure {
        java.srcDir(generatedDir)
    }
    doFirst {
        sourceSets.main.configure {
            val propertyFile = project.file("iconAccessorSpec.properties")
            val allIconsName = "AllIcons"
            generatedDir.mkdirs()
            val packageName = "com.github.weisj.darklaf.iconset"
            generatedDir.resolve("${packageName.replace('.', '/')}/$allIconsName.java").apply {
                parentFile.mkdirs()
                createNewFile()
                writeText(createIconAccessor(propertyFile, packageName, allIconsName))
            }
        }
    }
}

tasks.compileJava.configure {
    dependsOn(generateIconAccessor)
}
