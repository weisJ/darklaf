import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings

plugins {
    `java-library`
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
    val generatedDir = project.layout.buildDirectory.dir("generated/iconAccessor")
    sourceSets.main.configure {
        java.srcDir(generatedDir)
    }
    doFirst {
        sourceSets.main.configure {
            val generatedDirFile = generatedDir.get().asFile
            val propertyFile = project.file("iconAccessorSpec.properties")
            val allIconsName = "AllIcons"
            generatedDirFile.mkdirs()
            val packageName = "com.github.weisj.darklaf.iconset"
            generatedDirFile
                .resolve("${packageName.replace('.', '/')}/$allIconsName.java")
                .apply {
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
