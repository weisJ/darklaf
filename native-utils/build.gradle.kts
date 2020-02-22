plugins {
    `java-library`
}

dependencies {
    implementation("net.java.dev.jna:jna")
}

tasks.jar {
    com.github.vlsi.gradle.crlf.CrLfSpec(com.github.vlsi.gradle.crlf.LineEndings.LF).run {
        into("META-INF") {
            filteringCharset = "UTF-8"
            textFrom("$rootDir/licenses/NATIVEUTIL_LICENSE.txt")
        }
    }
}
