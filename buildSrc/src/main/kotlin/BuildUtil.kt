import java.io.File
import java.util.concurrent.TimeUnit

fun runCommand(vararg args : String, workingDirectory : File? = null): String? {
    return runCatching {
        val process = ProcessBuilder(*args).run {
            if (workingDirectory != null) directory(workingDirectory)
            start()
        }
        val output = process.inputStream.reader(Charsets.UTF_8).use {
            it.readText()
        }
        process.waitFor(10, TimeUnit.SECONDS)
        output.trim()
    }.getOrNull()
}

fun String.runCommand(): String? {
    return runCommand(*split(" ").toTypedArray())
}
