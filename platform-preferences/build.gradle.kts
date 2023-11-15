plugins {
    `java-library`
}

dependencies {
    api(projects.darklafThemeSpec)
    implementation(projects.darklafPlatformBase)
    implementation(projects.darklafWindows)
    implementation(projects.darklafMacos)
}
