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
