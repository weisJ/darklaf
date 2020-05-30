import dev.nokee.platform.jni.JniLibraryExtension
import groovy.transform.CompileStatic
import org.gradle.api.Action
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.plugins.ExtensionAware

@CompileStatic
class UsePrebuiltBinariesWhenUnbuildablePlugin implements Plugin<Project> {

    private PrebuildBinariesExtension prebuildExtension
    private GithubArtifactExtension githubArtifactExtension

    void prebuildBinaries(Action<? extends PrebuildBinariesExtension> action) {
        action.execute(prebuildExtension)
    }

    PrebuildBinariesExtension getPrebuildBinaries() {
        return prebuildExtension
    }

    @Override
    void apply(Project project) {
        JniLibraryExtension library = project.extensions.getByType(JniLibraryExtension)
        prebuildExtension = project.extensions.create("prebuildBinaries", PrebuildBinariesExtension, this)
        githubArtifactExtension = (prebuildExtension as ExtensionAware).with {
            it.extensions.create("github", GithubArtifactExtension)
        }
        library.variants.configureEach { var ->
            if (prebuildExtension.alwaysUsePrebuildArtifact || !var.sharedLibrary.buildable) {
                // Try to include the library file... if available
                def defaultLibraryName = JniUtils.getLibraryFileNameFor(project, var.targetMachine.operatingSystemFamily)
                def variantName = JniUtils.asVariantName(var.targetMachine)
                def libraryFile = project.file(
                        "${prebuildExtension.prebuildLibrariesFolder}/$variantName/$defaultLibraryName"
                )

                if (!libraryFile.exists()) {
                    // No local binary provided. Try to download it from github actions.
                    def prebuiltBinariesTask = project.tasks.register("downloadPrebuiltBinary$variantName", DownloadPrebuiltBinaryFromGitHubAction.class)
                    prebuiltBinariesTask.configure {
                        it.githubAccessToken = githubArtifactExtension.accessToken
                        it.variant = variantName
                        it.user = githubArtifactExtension.user
                        it.repository = githubArtifactExtension.repository
                        it.workflow = githubArtifactExtension.workflow
                        it.manualDownloadUrl = githubArtifactExtension.manualDownloadUrl
                        it.branches = githubArtifactExtension.branches
                        it.missingLibraryIsFailure = prebuildExtension.missingLibraryIsFailure
                    }
                    var.nativeRuntimeFiles.setFrom(prebuiltBinariesTask.map { it.prebuiltBinaryFile })
                    var.nativeRuntimeFiles.from(new CallableLogger({
                        project.logger.warn("${project.name}: Using pre-build library from github for targetMachine $variantName.")
                    }))
                } else {
                    //Use provided library.
                    var.nativeRuntimeFiles.setFrom(libraryFile)
                    var.nativeRuntimeFiles.from(new CallableLogger({
                        def relativePath = project.rootProject.relativePath(libraryFile)
                        project.logger.warn("${project.name}: Using pre-build library $relativePath for targetMachine $variantName.")
                    }))
                }
            }
        }
    }

    static class PrebuildBinariesExtension {

        private String prebuildLibrariesFolder = "pre-build-libraries"
        private boolean alwaysUsePrebuildArtifact = false
        private boolean missingLibraryIsFailure = true
        private UsePrebuiltBinariesWhenUnbuildablePlugin plugin

        PrebuildBinariesExtension(UsePrebuiltBinariesWhenUnbuildablePlugin plugin) {
            this.plugin = plugin
        }

        void github(Action<? extends GithubArtifactExtension> action) {
            action.execute(plugin.githubArtifactExtension)
        }

        void setAlwaysUsePrebuildArtifact(boolean alwaysUsePrebuildArtifact) {
            this.alwaysUsePrebuildArtifact = alwaysUsePrebuildArtifact
        }

        boolean getAlwaysUsePrebuildArtifact() {
            return alwaysUsePrebuildArtifact
        }

        String getPrebuildLibrariesFolder() {
            return prebuildLibrariesFolder
        }

        boolean getMissingLibraryIsFailure() {
            return missingLibraryIsFailure
        }

        void setPrebuildLibrariesFolder(String prebuildLibrariesFolder) {
            this.prebuildLibrariesFolder = prebuildLibrariesFolder
        }

        void setMissingLibraryIsFailure(boolean missingLibraryIsFailure) {
            this.missingLibraryIsFailure = missingLibraryIsFailure
        }
    }

    static class GithubArtifactExtension {
        private String user
        private String repository
        private String workflow
        private String manualDownloadUrl
        private String accessToken
        private List<String> branches = ["master"]

        String getUser() {
            return user
        }

        String getRepository() {
            return repository
        }

        String getWorkflow() {
            return workflow
        }

        String getManualDownloadUrl() {
            return manualDownloadUrl
        }

        String getAccessToken() {
            return accessToken
        }

        List<String> getBranches() {
            return branches
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

        void setAccessToken(String accessToken) {
            this.accessToken = accessToken
        }

        void setBranches(List<String> branches) {
            this.branches = branches
        }
    }

}
