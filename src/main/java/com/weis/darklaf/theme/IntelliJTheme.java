package com.weis.darklaf.theme;

import com.weis.darklaf.util.PropertyLoader;
import com.weis.darklaf.util.SystemInfo;

import javax.swing.*;
import java.util.Properties;

/**
 * @author Jannis Weis
 */
public class IntelliJTheme extends Theme {

    @Override
    public void loadPlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
        super.loadPlatformProperties(properties, currentDefaults);
        if (SystemInfo.isMac) {
            var name = getResourcePath() + getName() + "_mac.properties";
            PropertyLoader.putProperties(load(name), properties, currentDefaults);
        }
    }

    @Override
    protected String getResourcePath() {
        return "intellij/";
    }

    @Override
    public boolean isDark() {
        return false;
    }

    @Override
    public String getName() {
        return "intellij";
    }
}
