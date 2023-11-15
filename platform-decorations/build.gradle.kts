plugins {
    `java-library`
}

dependencies {
    api(projects.darklafPlatformBase)
    implementation(projects.darklafPropertyLoader)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
}
