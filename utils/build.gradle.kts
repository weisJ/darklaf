plugins {
    `java-library`
}

dependencies {
    implementation(projects.darklafPlatformBase)
    compileOnly(libs.nullabilityAnnotations)
    compileOnly(toolLibs.errorprone.annotations)

    testImplementation(testLibs.junit.api)
    testRuntimeOnly(testLibs.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
