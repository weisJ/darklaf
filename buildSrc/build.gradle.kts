plugins {
    `kotlin-dsl`
    `java-gradle-plugin`
}

dependencies {
    implementation(nokeeApi())
    implementation(gradleApi())
}

repositories {
    mavenCentral()
    gradlePluginPortal()
    nokee()
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
