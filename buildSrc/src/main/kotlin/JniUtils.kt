import dev.nokee.runtime.nativebase.OperatingSystemFamily
import dev.nokee.runtime.nativebase.TargetMachine
import org.gradle.api.GradleException
import org.gradle.api.Project

val TargetMachine.variantName: String
    get() {
        val osFamily = when {
            operatingSystemFamily.isWindows -> "windows"
            operatingSystemFamily.isLinux -> "linux"
            operatingSystemFamily.isMacOS -> "macos"
            else -> GradleException("Unknown operating system family '${operatingSystemFamily}'.")
        }
        val architecture = if (architecture.is32Bit) "x86" else "x86-64"
        return "$osFamily-$architecture"
    }

fun libraryFileNameFor(project : Project, osFamily: OperatingSystemFamily) : String = when {
    osFamily.isWindows -> "${project.name}.dll"
    osFamily.isLinux -> "lib${project.name}.so"
    osFamily.isMacOS -> "lib${project.name}.dylib"
    else -> throw GradleException("Unknown operating system family '${osFamily}'.")
}
