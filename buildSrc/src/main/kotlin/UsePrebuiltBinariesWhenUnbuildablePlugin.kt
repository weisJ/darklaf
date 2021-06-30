import dev.nokee.platform.jni.JavaNativeInterfaceLibrary
import org.gradle.api.Action
import org.gradle.api.Plugin
import org.gradle.api.Project
import dev.nokee.platform.jni.JniLibrary
import java.io.File

class UsePrebuiltBinariesWhenUnbuildablePlugin : Plugin<Project> {

    lateinit var prebuiltExtension: PrebuiltBinariesExtension

    fun prebuiltBinaries(action: Action<PrebuiltBinariesExtension>) {
        action.execute(prebuiltExtension)
    }

    override fun apply(target: Project) {
        prebuiltExtension = target.extensions.create("prebuiltBinaries", PrebuiltBinariesExtension::class.java)
        val library = target.extensions.getByType(JavaNativeInterfaceLibrary::class.java)
        library.variants.configureEach {
            if (prebuiltExtension.alwaysUsePrebuiltArtifact || !sharedLibrary.isBuildable) {
                configure(target, this)
            }
        }
    }

    private fun configure(project: Project, library: JniLibrary) {
        with(prebuiltExtension) {
            val defaultLibraryName = libraryFileNameFor(project, library.targetMachine.operatingSystemFamily)
            val variantName = library.targetMachine.variantName
            val libraryFile = project.file("$prebuiltLibrariesFolder/$variantName/$defaultLibraryName")

            if (libraryFile.exists()) {
                useLocalLibrary(project, library, libraryFile, variantName)
            } else {
                // No local binary provided. Try to download it from github actions.
                useGithubLibrary(project, library, variantName)
            }
        }
    }

    private fun useGithubLibrary(project: Project, library: JniLibrary, variantName: String) {
        val prebuiltBinariesTask = project.tasks.register(
            "downloadPrebuiltBinary$variantName",
            DownloadPrebuiltBinariesTask::class.java,
            variantName,
            prebuiltExtension
        )
        library.nativeRuntimeFiles.setFrom(prebuiltBinariesTask.map { it.getPrebuiltBinaryFile() })
        library.nativeRuntimeFiles.from(CallableAction {
            project.logger.warn(
                "${project.name}: Using pre-build library from github for targetMachine $variantName."
            )
        })
    }

    private fun useLocalLibrary(project: Project, library: JniLibrary, libraryFile: File, variantName: String) {
        library.nativeRuntimeFiles.setFrom(libraryFile)
        library.nativeRuntimeFiles.from(CallableAction {
            val relativePath = project.rootProject.relativePath(libraryFile)
            project.logger.warn(
                "${project.name}: Using pre-build library $relativePath for targetMachine $variantName."
            )
        })
    }
}

open class PrebuiltBinariesExtension {

    internal var githubArtifactSpec: GithubArtifactSpec? = null
    var prebuiltLibrariesFolder: String = "pre-build-libraries"
    var alwaysUsePrebuiltArtifact: Boolean = false
    var failIfLibraryIsMissing: Boolean = true

    fun github(user: String, repository: String, workflow: String, action: Action<GithubArtifactSpec>) {
        githubArtifactSpec = GithubArtifactSpec(user, repository, workflow).also { action.execute(it) }
    }
}

data class GithubArtifactSpec(
    var user: String,
    var repository: String?,
    var workflow: String,
    var manualDownloadUrl: String = "",
    var accessToken: String? = null,
    var timeout: Int = 0,
    var branches: List<String> = listOf("master")
)
