import dev.nokee.platform.jni.JarBinary
import dev.nokee.platform.jni.JniJarBinary
import dev.nokee.platform.jni.JniLibraryExtension
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.file.FileTree
import org.gradle.api.provider.Provider
import org.gradle.jvm.tasks.Jar

class UberJniJarPlugin : Plugin<Project> {

    override fun apply(target: Project) {
        target.tasks.named("jar", Jar::class.java) {
            configure(this)
        }
    }

    private fun configure(task: Jar) {
        val project = task.project
        val logger = task.logger
        val library = project.extensions.getByType(JniLibraryExtension::class.java)

        logger.info("${project.name}: Merging binaries into the JVM Jar.")
        task.from(library.variants.flatMap { variant ->
            if (variant.targetMachine.targetsHost) {
                variant.binaries.withType(JniJarBinary::class.java)
                    .map { it.asZipTree(project) }.get()
            } else listOf()
        })
    }

    private fun JarBinary.asZipTree(project: Project): Provider<FileTree> =
        jarTask.map { project.zipTree(it.archiveFile) }

}
