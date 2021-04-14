import dev.nokee.platform.jni.JniLibraryDependencies
import dev.nokee.runtime.nativebase.OperatingSystemFamily
import dev.nokee.runtime.nativebase.TargetMachine
import org.gradle.api.Action
import org.gradle.api.GradleException
import org.gradle.api.Project
import org.gradle.api.artifacts.MinimalExternalModuleDependency
import org.gradle.api.artifacts.ModuleDependency
import org.gradle.api.artifacts.ModuleDependencyCapabilitiesHandler
import org.gradle.api.provider.Provider

fun MinimalExternalModuleDependency.dependencyNotation() =
    "${module.group}:${module.name}:${versionConstraint.requiredVersion}"

fun JniLibraryDependencies.jvmLibImplementation(notation: Provider<MinimalExternalModuleDependency>) {
    jvmImplementation(notation.map { it.dependencyNotation() }.get())
}

fun JniLibraryDependencies.nativeLibImplementation(notation: Provider<MinimalExternalModuleDependency>) {
    nativeImplementation(notation.map { it.dependencyNotation() }.get())
}

fun JniLibraryDependencies.nativeLibImplementation(
    notation: Provider<MinimalExternalModuleDependency>,
    action: Action<in ModuleDependency>
) {
    nativeImplementation(notation.map { it.dependencyNotation() }.get(), action)
}

fun ModuleDependencyCapabilitiesHandler.requireLibCapability(notation: Provider<MinimalExternalModuleDependency>) {
    requireCapabilities(notation.get().dependencyNotation())
}

val TargetMachine.variantName: String
    get() {
        val osFamily = when {
            operatingSystemFamily.isWindows -> "windows"
            operatingSystemFamily.isLinux -> "linux"
            operatingSystemFamily.isMacOS -> "macos"
            else -> GradleException("Unknown operating system family '${operatingSystemFamily}'.")
        }
        return "$osFamily-$architectureString"
    }

val TargetMachine.targetsHost: Boolean
    get() {
        val osName = System.getProperty("os.name").toLowerCase().replace(" ", "")
        val osFamily = operatingSystemFamily
        return when {
            osFamily.isWindows && osName.contains("windows") -> true
            osFamily.isLinux && osName.contains("linux") -> true
            osFamily.isMacOS && osName.contains("macos") -> true
            else -> false
        }
    }

val TargetMachine.architectureString: String
    get() = if (architecture.is32Bit) "x86" else "x86-64"

fun libraryFileNameFor(project: Project, osFamily: OperatingSystemFamily): String = when {
    osFamily.isWindows -> "${project.name}.dll"
    osFamily.isLinux -> "lib${project.name}.so"
    osFamily.isMacOS -> "lib${project.name}.dylib"
    else -> throw GradleException("Unknown operating system family '${osFamily}'.")
}
