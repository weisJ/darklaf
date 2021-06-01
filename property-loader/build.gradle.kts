plugins {
    `java-library`
}

dependencies {
    api(projects.darklafUtils)
    implementation(libs.svgSalamander)
    implementation(libs.visualPaddings)
    implementation(libs.nullabilityAnnotations)
    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
