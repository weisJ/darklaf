import dev.nokee.platform.nativebase.OperatingSystemFamily
import dev.nokee.platform.nativebase.TargetMachine
import org.gradle.api.GradleException
import org.gradle.api.Project

class JniUtils {
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

    static String getLibraryFileNameFor(Project project, OperatingSystemFamily osFamily) {
        if (osFamily.windows) {
            return "${project.name}.dll"
        } else if (osFamily.linux) {
            return "lib${project.name}.so"
        } else if (osFamily.macOS) {
            return "lib${project.name}.dylib"
        }
        throw new GradleException("Unknown operating system family '${osFamily}'.")
    }
}
