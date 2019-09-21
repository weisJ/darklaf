package com.weis.darklaf.icons;

import javax.swing.*;

public class LazyImageIcon extends LazyIcon {

    public LazyImageIcon(final String path, final IconLoader.IconKey key, final Class<?> parentClass) {
        super(path, key, parentClass);
    }

    @Override
    protected Icon loadIcon() {
        return IconLoader.get(parentClass).createImageIcon(path, path);
    }
}
