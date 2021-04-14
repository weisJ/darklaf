import org.gradle.util.VersionNumber
import java.util.concurrent.TimeUnit

fun String.runCommand(): String? {
    return runCatching {
        val process = ProcessBuilder(*split(" ").toTypedArray()).start()
        val output = process.inputStream.reader(Charsets.UTF_8).use {
            it.readText()
        }
        process.waitFor(10, TimeUnit.SECONDS)
        output.trim()
    }.getOrNull()
}

fun getXCodeVersion(): VersionNumber? {
    val version = "/usr/bin/xcodebuild -version".runCommand() ?: return null
    val extracted = Regex("""Xcode\s+([0-9\\.]*).*""")
        .find(version)?.groupValues?.getOrNull(1) ?: return null
    return VersionNumber.parse(extracted)
}
