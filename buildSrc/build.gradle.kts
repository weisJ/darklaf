plugins {
    `java-gradle-plugin`
    groovy
}

dependencies {
    implementation(platform("dev.nokee:nokee-gradle-plugins:0.4.0-80d5445"))
}

repositories {
    mavenCentral()
    gradlePluginPortal()
    maven { url = uri("https://dl.bintray.com/nokeedev/distributions") }
    maven { url = uri("https://dl.bintray.com/nokeedev/distributions-snapshots") }
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
    }
}
