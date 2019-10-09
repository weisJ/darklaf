package com.weis.darklaf.theme;

/**
 * @author Jannis Weis
 */
public class IntelliJTheme extends Theme {

    @Override
    public String getName() {
        return "IntelliJ";
    }

    @Override
    protected String getResourcePath() {
        return "intellij/";
    }

    @Override
    public boolean isDark() {
        return false;
    }
}
