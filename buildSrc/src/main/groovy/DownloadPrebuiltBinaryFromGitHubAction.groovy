import groovy.json.JsonOutput
import groovy.json.JsonSlurper
import groovy.transform.CompileStatic
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.Transformer
import org.gradle.api.tasks.OutputFile

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.stream.Stream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile

@CompileStatic
class DownloadPrebuiltBinaryFromGitHubAction extends DefaultTask {

    private static final ReadWriteLock LOCK = new ReentrantReadWriteLock()

    private static final String VERSION_INFO_FILE_NAME = "github_artifact_versions.json"
    private static final String TEMP_PATH = "tmp${File.separator}prebuild"
    private static final String PRE_BUILD_PATH = "libs${File.separator}prebuild"

    private final OneTimeLogger tokenWarning = new OneTimeLogger({
        error("""No github access token is specified. Latest artifacts will need to be included manually.
              |The access token needs to have the 'read-public' property. Specify using:
              |    -PgithubAccessToken=<your token>
              |or by setting
              |    githubAccessToken=<your token>
              |inside the gradle.properties file.
              |""".stripMargin())
    })
    private final OneTimeLogger useCachedWarning = new OneTimeLogger({
        log("Could not download artifact or artifact information. Using cached version")
    })

    private Map cacheInfo

    private String manualDownloadUrl = ""
    private String user
    private String repository
    private String workflow
    private List<String> branches = []
    private boolean missingLibraryIsFailure

    private String githubAccessToken
    private String variant
    private Optional<File> prebuiltBinary

    @OutputFile
    File getPrebuiltBinaryFile() {
        if (user == null) throw new GradleException("Github user isn't specified")
        if (repository == null) repository = project.name
        if (workflow == null) throw new GradleException("Workflow isn't specified")

        if (prebuiltBinary == null) {
            if (githubAccessToken == null || githubAccessToken.isEmpty()) {
                tokenWarning.log()
            }
            prebuiltBinary = getExternalBinary(variant)
        }

        return prebuiltBinary.orElseGet {
            String errorMessage = """${project.name}: Library for $variant could not be downloaded.
                      |${(" " * (project.name.size() + 1))} Download it from $manualDownloadUrl
                      |""".stripMargin()
            if (missingLibraryIsFailure) {
                throw new GradleException(errorMessage)
            } else {
                error(errorMessage)
            }
            return createDirectory(tempFilePath("dummy/"))
        }
    }

    void setMissingLibraryIsFailure(boolean missingLibraryIsFailure) {
        this.missingLibraryIsFailure = missingLibraryIsFailure
    }

    void setGithubAccessToken(String githubAccessToken) {
        this.githubAccessToken = githubAccessToken
    }

    void setVariant(String variant) {
        this.variant = variant
    }

    void setUser(String user) {
        this.user = user
    }

    void setRepository(String repository) {
        this.repository = repository
    }

    void setWorkflow(String workflow) {
        this.workflow = workflow
    }

    void setManualDownloadUrl(String manualDownloadUrl) {
        this.manualDownloadUrl = manualDownloadUrl
    }

    void setBranches(List<String> branches) {
        this.branches = branches
    }

    private Map getCacheInfo() {
        if (cacheInfo == null) {
            LOCK.readLock().lock()
            try {
                File cacheInfoFile = getCacheInfoFile()
                JsonSlurper jsonParser = new JsonSlurper()
                cacheInfo = jsonParser.parseText(cacheInfoFile.text) as Map
            } finally {
                LOCK.readLock().unlock()
            }
        }
        return cacheInfo
    }

    private File getCacheInfoFile() {
        String path = preBuildPath(VERSION_INFO_FILE_NAME)
        File cacheInfo = new File(path)
        if (!cacheInfo.exists()) {
            cacheInfo = createFile(path)
            cacheInfo << "{}"
        }
        return cacheInfo
    }

    private void writeToCache(String variantName, String timeStamp, File file) {
        LOCK.writeLock().lock()
        try {
            Map cacheInfo = getCacheInfo()
            Map entry = [timeStamp: timeStamp, path: file.absolutePath]
            cacheInfo.put(variantName, entry)
            getCacheInfoFile().write(JsonOutput.prettyPrint(JsonOutput.toJson(cacheInfo)))
        } finally {

        }
    }

    Optional<File> getExternalBinary(String variant) {
        Tuple2<Optional<DownloadInfo>, Optional<File>> fetchResult = getBinaryDownloadUrl(variant)
        Optional<DownloadInfo> downloadInfo = fetchResult.getFirst()
        Optional<File> cachedFile = fetchResult.getSecond()
        if (cachedFile.isPresent()) {
            log("Reusing previously downloaded binary ${cachedFile.map { it.absolutePath }.orElse(null)}")
            return cachedFile
        }
        Optional<File> downloadedFile = downloadInfo.map {
            getBinaryFromUrl(variant, it.url).orElse(null)
        }

        if (downloadedFile.isPresent()) {
            writeToCache(variant, downloadInfo.get()?.timeStamp, downloadedFile.get())
        } else {
            info("No file found for variant $variant")
        }

        if (downloadedFile.isPresent()) return downloadedFile
        return getCachedFile(variant)
    }

    private Optional<File> getBinaryFromUrl(String variant, String url) {
        File directory = createDirectory(preBuildPath(variant))
        info("Downloading binary for variant '$variant' from $url")
        Optional<File> file = downloadZipFile(url, variant).map { unzip(it, directory).findFirst() }.orElse(Optional.empty())
        info("Finished download for variant '$variant'")
        return file
    }

    private String preBuildPath(String variant) {
        return "${project.buildDir}${File.separator}$PRE_BUILD_PATH${File.separator}$variant"
    }

    private Optional<ZipFile> downloadZipFile(String url, String variant) {
        return fetch(url) {
            File file = createFile(zipPath(variant))
            Path response = file.toPath()
            Files.copy(it.getInputStream(), response, StandardCopyOption.REPLACE_EXISTING)
            return new ZipFile(file)
        }
    }

    private String zipPath(String name) {
        return tempFilePath("${name}.zip")
    }

    private String tempFilePath(String name) {
        return "$project.buildDir${File.separator}$TEMP_PATH${File.separator}${name}"
    }

    private static Stream<File> unzip(ZipFile self, File directory) {
        Collection<ZipEntry> files = self.entries().findAll { !(it as ZipEntry).directory }
        return files.stream().map {
            ZipEntry e = it as ZipEntry
            e.name.with { fileName ->
                File outputFile = createFile("${directory.path}$File.separator$fileName")
                Files.copy(self.getInputStream(e), outputFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
                return outputFile
            }
        }
    }

    private Tuple2<Optional<DownloadInfo>, Optional<File>> getBinaryDownloadUrl(String variantName) {
        boolean isUptoDate = false
        File cachedFile = null
        String timeStamp = null
        String artifactUrl = getLatestRun(getJson(getWorkflowsUrl())).with {
            timeStamp = it.get("created_at")
            Optional<String> cachedFilePath = getCachedFilePath(variantName, timeStamp)
            isUptoDate = cachedFilePath.isPresent()
            if (isUptoDate) {
                cachedFile = new File(cachedFilePath.get())
                isUptoDate = cachedFile.exists()
            }
            return get("artifacts_url") as String
        }
        info("Latest artifact for variant '$variantName' is from $timeStamp")
        if (isUptoDate) {
            return new Tuple2<>(Optional.empty(), Optional.of(cachedFile))
        }
        DownloadInfo downloadInfo = artifactUrl?.with { url ->
            Map[] artifacts = getJson(url).get("artifacts") as Map[]
            String artifactDownloadUrl = artifacts?.find { variantName == it.get("name") }?.get("url") as String
            return artifactDownloadUrl?.with {
                new DownloadInfo(getJson(it)?.get("archive_download_url") as String, timeStamp)
            }
        }

        return new Tuple2<>(Optional.ofNullable(downloadInfo), Optional.empty())
    }

    private String getWorkflowsUrl() {
        return "https://api.github.com/repos/$user/$repository/actions/workflows/$workflow/runs"
    }

    private Optional<String> getCachedFilePath(String variantName, String timeStamp) {
        Map cacheInfo = getCacheInfo()
        boolean isLatest = (cacheInfo[variantName] as Map)?.get("timeStamp") == timeStamp
        if (isLatest) {
            return Optional.ofNullable((cacheInfo[variantName] as Map)?.get("path") as String)
        } else {
            return Optional.empty()
        }
    }

    private Optional<File> getCachedFile(String variant) {
        Map cacheInfo = getCacheInfo()
        return Optional.ofNullable(cacheInfo[variant] as Map).map {
            return new File(String.valueOf(it["path"])).with {
                if (it.exists()) useCachedWarning.log()
                it.exists() ? it : null
            }
        }
    }

    private Map getLatestRun(Map json) {
        Map[] runs = json.get("workflow_runs") as Map[]
        return Optional.ofNullable(runs?.find { run ->
            boolean completed = "completed" == run.get("status")
            boolean success = "success" == run.get("conclusion")
            boolean isCorrectBranch = branches.isEmpty() || branches.contains(run.get("head_branch")?.toString())
            return completed && success && isCorrectBranch
        }).orElseGet {
            log("No suitable workflow run found.")
            return Collections.emptyMap()
        }
    }

    private Map getJson(String url) {
        return fetch(url) {
            JsonSlurper jsonParser = new JsonSlurper()
            Map parsedJson = jsonParser.parseText(it.getInputStream().getText()) as Map
            return parsedJson
        }.orElse(Collections.emptyMap())
    }

    private <T> Optional<T> fetch(String url, Transformer<T, HttpURLConnection> transformer) {
        info("Fetching $url")
        if (isOffline()) return Optional.empty()
        HttpURLConnection get = new URL(url).openConnection() as HttpURLConnection
        get.setRequestMethod("GET")
        githubAccessToken?.with {
            get.setRequestProperty("Authorization", "token $it")
        }
        try {
            def responseCode = get.getResponseCode()
            if (responseCode == HttpURLConnection.HTTP_OK) {
                return Optional.ofNullable(transformer.transform(get))
            } else {
                log("Could not fetch $url. Response code '$responseCode'.")
            }
        } catch (IOException ignored) {
        }
        return Optional.empty()
    }

    private static File createFile(String fileName) {
        File file = new File(fileName)
        if (file.exists()) file.delete()
        file.getParentFile().mkdirs()
        file.createNewFile()
        return file
    }

    private static File createDirectory(String fileName) {
        File file = new File(fileName)
        file.mkdirs()
        return file
    }

    private boolean isOffline() {
        return project.getGradle().startParameter.isOffline()
    }

    private void info(String message) {
        project.logger.info("${project.name}: $message")
    }

    private void log(String message) {
        project.logger.warn("${project.name}: $message")
    }

    private void error(String message) {
        project.logger.error("${project.name}: $message")
    }

    private class DownloadInfo {
        protected String url
        protected String timeStamp

        private DownloadInfo(String url, String timeStamp) {
            this.url = url
            this.timeStamp = timeStamp
        }
    }
}
