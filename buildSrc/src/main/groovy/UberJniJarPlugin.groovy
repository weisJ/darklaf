import dev.nokee.platform.jni.JniJarBinary
import dev.nokee.platform.jni.JniLibraryExtension
import dev.nokee.platform.nativebase.TargetMachine
import groovy.transform.CompileStatic
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.file.CopySpec
import org.gradle.api.file.FileTree
import org.gradle.api.logging.Logger
import org.gradle.api.tasks.util.PatternFilterable
import org.gradle.jvm.tasks.Jar

@CompileStatic
class UberJniJarPlugin implements Plugin<Project> {
    @Override
    void apply(Project project) {
        project.tasks.named('jar', Jar) { task ->
            configure(task)
        }
    }

    private void configure(Jar task) {
        def project = task.getProject()
        def logger = task.getLogger()
        def library = project.extensions.getByType(JniLibraryExtension)
        def buildableVariants = library.variants.elements.get()
        if (buildableVariants.empty) {
            usePrebuiltBinaryIfAvailable(project, library, logger, task)
        } else if (buildableVariants.size() > 1) {
            includeBuiltJniJarContent(logger, project, task, library)
        }
    }

    private void includeBuiltJniJarContent(Logger logger, Project project, Jar task, JniLibraryExtension library) {
        logger.info("${project.name}: Merging binaries into the JVM Jar.")
        // There is no need to specify the destination of the content as it's already configured via library.resourcePath property
        task.from(library.binaries.withType(JniJarBinary).elements.map { Set<JniJarBinary> binaries ->
            binaries*.jarTask*.map(jniLibraryBinaryFiles(project))
        })
    }

    private static Closure<FileTree> jniLibraryBinaryFiles(Project project) {
        return { Jar task ->
            project.zipTree(task.archiveFile).matching { PatternFilterable p -> p.exclude('META-INF/**/*') }
        }
    }

    private void usePrebuiltBinaryIfAvailable(Project project, JniLibraryExtension library, Logger logger, Jar task) {
        def downloadUrl = 'https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22+branch%3Amaster'
        def defaultLibraryName = project.property('defaultLibraryName')
        for (TargetMachine targetMachine : library.targetMachines.get()) {
            def libraryPath = "com/github/weisj/darklaf/platform/${project.name}"
            def variantName = asVariantName(targetMachine)
            def libraryFile = project.file("libraries/$variantName/$defaultLibraryName")
            def relativePath = project.rootProject.relativePath(libraryFile)
            if (!libraryFile.exists()) {
                logger.warn("""${project.name}: Library $relativePath for targetMachine $variantName does not exist.
                            |${" ".multiply(project.name.size() + 1)} Download it from $downloadUrl
                            |""".stripMargin())
            } else {
                //Use provided library.
                logger.warn("${project.name}: Using pre-build library $relativePath for targetMachine $variantName.")
                task.into("$libraryPath/$variantName") { CopySpec spec ->
                    spec.from(libraryFile)
                }
            }
        }
    }

    static String asVariantName(TargetMachine targetMachine) {
        String operatingSystemFamily = 'macos'
        if (targetMachine.operatingSystemFamily.windows) {
            operatingSystemFamily = 'windows'
        }

        String architecture = 'x86-64'
        if (targetMachine.architecture.'32Bit') {
            architecture = 'x86'
        }

        return "$operatingSystemFamily-$architecture"
    }
}
