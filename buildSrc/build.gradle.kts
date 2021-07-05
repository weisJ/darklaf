apply(from= "../gradle/loadProps.gradle.kts")

plugins {
    `kotlin-dsl`
    `java-gradle-plugin`
}

val nokeeVersion = extra["nokee.version"]

dependencies {
    implementation(platform("dev.nokee:nokee-gradle-plugins:$nokeeVersion"))
    implementation(gradleApi())
}

repositories {
    mavenCentral()
    gradlePluginPortal()
    maven { url = uri("https://repo.nokee.dev/release") }
    maven { url = uri("https://repo.nokee.dev/snapshot") }
}

gradlePlugin {
    plugins {
        create("uber-jni-jar") {
            id = "uber-jni-jar"
            implementationClass = "UberJniJarPlugin"
        }
        create("use-prebuilt-binaries") {
            id = "use-prebuilt-binaries"
            implementationClass = "UsePrebuiltBinariesWhenUnbuildablePlugin"
        }
        create("apple-m1-toolchain") {
            id = "apple-m1-toolchain"
            implementationClass = "AppleM1ToolChainRule"
        }
    }
}
