import groovy.json.JsonOutput
import groovy.json.JsonSlurper
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.OutputFile
import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import javax.inject.Inject

private typealias Json = Map<String, Any>

@Suppress("UNCHECKED_CAST")
open class DownloadPrebuiltBinariesTask @Inject constructor(
    private val variantName: String,
    private val extension: PrebuiltBinariesExtension
) : DefaultTask() {

    companion object {
        private val LOCK: ReadWriteLock = ReentrantReadWriteLock()
        private const val VERSION_INFO_FILE_NAME = "github_artifact_versions.json"
        private const val TEMP_PATH = "tmp/prebuild"
        private const val PRE_BUILD_PATH = "libs/prebuild"
    }

    private val githubArtifactSpec = extension.githubArtifactSpec ?: throw GradleException("Github is not configured.")

    private val isOffline
        get() = project.gradle.startParameter.isOffline

    private val workflowURL
        get() = with(githubArtifactSpec) {
            URL("https://api.github.com/repos/$user/$repository/actions/workflows/$workflow/runs")
        }

    private val prebuiltDirectoryPath = "${project.buildDir}/$PRE_BUILD_PATH/$variantName"

    private val cacheFile: File by lazy {
        val cachePath = "${project.buildDir}/$PRE_BUILD_PATH/$VERSION_INFO_FILE_NAME"
        fileOf(cachePath).also { it.writeText("{}") }
    }
    private val cache: Json by lazy { LOCK.read { cacheFile.readText().toJson() } }

    private val prebuiltBinary: File? by lazy { fetchBinaryFile() }

    @OutputFile
    fun getPrebuiltBinaryFile(): File {
        return prebuiltBinary ?: run {
            val errorMessage = """
                        Library for $variantName could not be downloaded.
                        Download it from ${githubArtifactSpec.manualDownloadUrl}
                    """.trimIndent()
            if (extension.failIfLibraryIsMissing) {
                throw GradleException(errorMessage)
            } else {
                OneTimeAction.createGlobal("$variantName-missing") {
                    errorLog(errorMessage)
                }.execute()
            }
            directoryOf(tempFilePath("dummy/"))
        }
    }

    private fun <T> fetchFailed(message: String = ""): T? {
        errorLog(message)
        return null
    }

    private fun fetchBinaryFile(): File? {
        val run = workflowURL.getJson().latestRun
            ?: return fetchFailed("Could not get latest run")
        val timeStamp = run["created_at"]
        val cachedPathTimeStamp = cache["timeStamp"]
        infoLog("Latest artifact for variant '$variantName' is from $timeStamp")
        if (timeStamp == cachedPathTimeStamp) {
            val cachedFile = File(cache["path"].toString())
            if (cachedFile.exists()) {
                warnLog("Reusing previously downloaded binary ${cachedFile.absolutePath}")
                return cachedFile
            }
        }
        val artifactUrl = run["artifacts_url"]?.toString()
            ?: return fetchFailed("Could not get artifacts urls")
        val artifacts = URL(artifactUrl).getJson()["artifacts"] as List<Json>
        val downloadUrl = artifacts.find { variantName == it["name"] }?.get("url")?.toString()
            ?: return fetchFailed("Could not find matching artifact for $variantName")
        val artifactDownloadUrl = URL(downloadUrl).getJson()["archive_download_url"]?.toString()
            ?: return fetchFailed("Could not get download url")
        val artifact = downloadBinary(artifactDownloadUrl)
        if (artifact != null) {
            LOCK.write {
                val mutableCache = cache.toMutableMap()
                mutableCache["timeStamp"] = timeStamp ?: ""
                mutableCache["path"] = artifact.absolutePath
                cacheFile.writeText(JsonOutput.prettyPrint(JsonOutput.toJson(mutableCache)))
            }
        }
        return artifact
    }

    private fun downloadBinary(url: String): File? {
        infoLog("Downloading binary for variant '$variantName' from $url")
        return URL(url).fetch {
            val artifact = fileOf(tempFilePath("$variantName.zip"))
            Files.copy(it.inputStream, artifact.toPath(), StandardCopyOption.REPLACE_EXISTING)
            infoLog("Finished download for variant '$variantName'")
            ZipFile(artifact).unzip(directoryOf(prebuiltDirectoryPath)).firstOrNull()
        }
    }

    private val Json.latestRun: Json?
        get() {
            val runs = this["workflow_runs"] as? List<Json> ?: return null
            val candidates = runs.asSequence().filter {
                val completed = "completed" == it["status"]
                val success = "success" == it["conclusion"]
                completed && success
            }
            val branches = githubArtifactSpec.branches
            if (branches.isEmpty()) return candidates.firstOrNull()
            return branches.asSequence().mapNotNull { branch ->
                candidates.find { branch == it["head_branch"] }
            }.firstOrNull()
        }

    private fun URL.getJson(): Json = fetch { connection ->
        connection.inputStream.bufferedReader().use { it.readText() }.toJson()
    } ?: emptyMap()

    private fun <T : Any> URL.fetch(transform: (HttpURLConnection) -> T?): T? {
        if (isOffline) return null
        infoLog("Fetching $this")
        (openConnection() as HttpURLConnection).run {
            requestMethod = "GET"
            if (githubArtifactSpec.timeout >= 0) {
                connectTimeout = githubArtifactSpec.timeout
            }
            githubArtifactSpec.accessToken?.also {
                setRequestProperty("Authorization", "token $it")
            }
            return runCatching {
                when (responseCode) {
                    HttpURLConnection.HTTP_OK -> return transform(this)
                    else -> error("Could not fetch $url. Response code '$responseCode'.")
                }
            }.getOrElse {
                errorLog(it.message ?: "")
                null
            }
        }
    }

    private fun tempFilePath(name: String) = "${project.buildDir}/$TEMP_PATH/${name}"

    private fun directoryOf(fileName: String) = File(fileName).also { it.mkdirs() }

    private fun fileOf(fileName: String): File {
        val file = File(fileName)
        if (!file.exists()) {
            file.parentFile.mkdirs()
            file.createNewFile()
        }
        return file
    }

    private fun infoLog(message: String) = project.logger.info(message.format())
    private fun warnLog(message: String) = project.logger.warn(message.format())
    private fun errorLog(message: String) = project.logger.error(message.format())

    private fun String.format(): String {
        val pad = " ".repeat(project.name.length + 2)
        return "${project.name}: ${replace("\n", "\n$pad")}"
    }

    private fun String.toJson(): Json =
        JsonSlurper().parseText(this) as Json

    private fun <T> Lock.use(action: () -> T): T {
        lock()
        try {
            return action()
        } catch (e: Exception) {
            unlock()
            throw e
        } finally {
            unlock()
        }
    }

    private fun <T> ReadWriteLock.read(action: () -> T): T = readLock().use(action)
    private fun <T> ReadWriteLock.write(action: () -> T): T = writeLock().use(action)

    private fun ZipFile.unzip(directory: File): Sequence<File> {
        return entries().asSequence()
            .map { it as ZipEntry }
            .filter { !it.isDirectory }
            .map {
                val entryFile = fileOf("${directory.path}/${it.name}")
                Files.copy(getInputStream(it), entryFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
                entryFile
            }
    }
}
