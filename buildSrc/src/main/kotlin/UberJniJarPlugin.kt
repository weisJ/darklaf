import dev.nokee.platform.base.VariantView
import dev.nokee.platform.jni.JniJarBinary
import dev.nokee.platform.jni.JniLibrary
import dev.nokee.platform.jni.JniLibraryExtension
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.provider.Provider
import org.gradle.jvm.tasks.Jar
import dev.nokee.runtime.nativebase.TargetMachine

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
        library.binaries.withType(JniJarBinary::class.java).configureEach {
            jarTask.configure { enabled = false }
        }
        logger.info("${project.name}: Merging binaries into the JVM Jar.")
        when (library.targetMachines.get().size) {
            0 -> logger.info("No native target for project ${project.name}")
            1 -> {
                library.variants.configureEach {
                    task.into(this@configureEach.resourcePath) {
                        from(this@configureEach.nativeRuntimeFiles)
                    }
                }
            }
            else -> {
                for (targetMachine in library.targetMachines.get()) {
                    val variant = library.variants.withTarget(targetMachine)
                    task.into(variant.map { it.resourcePath }) {
                        from(variant.map { it.nativeRuntimeFiles })
                    }
                }
            }
        }
    }

    private fun VariantView<JniLibrary>.withTarget(target: TargetMachine): Provider<JniLibrary> {
        return filter { it.targetMachine == target }.map {
            check(it.size == 1)
            it.first()
        }
    }
}
