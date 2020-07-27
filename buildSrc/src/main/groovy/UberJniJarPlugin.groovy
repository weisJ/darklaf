import dev.nokee.platform.jni.JniJarBinary
import dev.nokee.platform.jni.JniLibrary
import dev.nokee.platform.jni.JniLibraryExtension
import dev.nokee.runtime.nativebase.TargetMachine
import groovy.transform.CompileStatic
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.Transformer
import org.gradle.api.file.CopySpec
import org.gradle.api.provider.Provider
import org.gradle.jvm.tasks.Jar

@CompileStatic
class UberJniJarPlugin implements Plugin<Project> {

    @Override
    void apply(Project project) {
        project.tasks.named('jar', Jar) { task ->
            configure(task)
        }
    }

    private static void configure(Jar task) {
        def project = task.getProject()
        def logger = task.getLogger()
        def library = project.extensions.getByType(JniLibraryExtension)
        library.binaries.withType(JniJarBinary).configureEach {
            if (it.jarTask.isPresent()) it.jarTask.get()?.enabled = false
        }
        logger.info("${project.name}: Merging binaries into the JVM Jar.")
        if (library.targetMachines.get().size() > 1) {
            for (TargetMachine targetMachine : library.targetMachines.get()) {
                Provider<JniLibrary> variant = library.variants
                        .flatMap(targetMachineOf(targetMachine))
                        .map(onlyOne() as Transformer<?, ? super List<?>>) as Provider<JniLibrary>
                task.into(variant.map { it.resourcePath }) { CopySpec spec ->
                    spec.from(variant.map { it.nativeRuntimeFiles })
                }
            }
        } else {
            library.variants.configureEach {
                task.into(it.resourcePath) { CopySpec spec ->
                    spec.from(it.nativeRuntimeFiles)
                }
            }
        }
    }

    // Filter variants that match the specified target machine.
    private static Transformer<Iterable<JniLibrary>, JniLibrary> targetMachineOf(TargetMachine targetMachine) {
        return new Transformer<Iterable<JniLibrary>, JniLibrary>() {
            @Override
            Iterable<JniLibrary> transform(JniLibrary variant) {
                if (variant.targetMachine == targetMachine) {
                    return [variant]
                }
                return []
            }
        }
    }

    // Ensure only a single variant is present in the collection and return the variant.
    private static Transformer<JniLibrary, List<? extends JniLibrary>> onlyOne() {
        return new Transformer<JniLibrary, List<? extends JniLibrary>>() {
            @Override
            JniLibrary transform(List<? extends JniLibrary> variants) {
                assert variants.size() == 1
                return variants.first()
            }
        }
    }
}
