package com.github.weisj.darklaf.core.test;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.iconset.IconSet;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import javax.swing.UIDefaults;
import java.util.function.Supplier;

class IconCacheTest {

    @Test
    @Timeout(value = 15)
    void testIconCacheGetsReleased() {
        UIDefaults defaults = new DarkLaf().getDefaults();
        Assertions.assertFalse(IconSet.ICON_LOADER.isCacheEmpty());
        defaults.clear();
        waitForGarbageCollection(() -> !IconSet.ICON_LOADER.isCacheEmpty());
    }

    @SuppressWarnings({"unused"})
    private void waitForGarbageCollection(final Supplier<Boolean> checker) {
        while (checker.get()) {
            Object object = null;
            try {
                object = new int[10][10][10][10][10][10][10][10][10][10][10][10];
            } catch (OutOfMemoryError ignored) {
            }
            System.out.println(object);
            System.gc();
        }
    }

}
