/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.net.URL;
import java.util.Locale;
import java.util.function.Supplier;

import javax.accessibility.*;
import javax.swing.*;

public class DerivableImageIcon implements DerivableIcon<DerivableImageIcon>, Accessible {

    private static final int DEFAULT_SCALING_MODE = Image.SCALE_SMOOTH;
    private final int scalingMode;

    private int width;
    private int height;

    private Supplier<Image> imageSupplier;
    private Image original;
    private Image img;
    private String description;
    private AccessibleContext accessibleContext;

    /**
     * Create a new derivable image icon.
     *
     * @param imagePath the image path.
     */
    public DerivableImageIcon(final String imagePath) {
        this(() -> Toolkit.getDefaultToolkit().getImage(imagePath), -1, -1);
    }

    /**
     * Create a new derivable image icon.
     *
     * @param imagePath the image path.
     * @param width     the icon width.
     * @param height    the icon height.
     */
    public DerivableImageIcon(final String imagePath, final int width, final int height) {
        this(imagePath, width, height, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon.
     *
     * @param imagePath   the image path.
     * @param width       the icon width.
     * @param height      the icon height.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final String imagePath, final int width, final int height, final int scalingMode) {
        this(() -> Toolkit.getDefaultToolkit().getImage(imagePath), width, height, scalingMode);
    }

    /**
     * Create a new derivable image icon.
     *
     * @param url the url to load the image from.
     */
    public DerivableImageIcon(final URL url) {
        this(url, -1, -1, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon.
     *
     * @param url    the url to load the image from.
     * @param width  the icon width.
     * @param height the icon height.
     */
    public DerivableImageIcon(final URL url, final int width, final int height) {
        this(url, width, height, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon.
     *
     * @param url         the url to load the image from.
     * @param width       the icon width.
     * @param height      the icon height.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final URL url, final int width, final int height, final int scalingMode) {
        this(() -> Toolkit.getDefaultToolkit().getImage(url), width, height, scalingMode);
    }

    public DerivableImageIcon(final Supplier<Image> imageSupplier) {
        this(imageSupplier, -1, -1, DEFAULT_SCALING_MODE);
    }

    public DerivableImageIcon(final Supplier<Image> imageSupplier, final int width, final int height) {
        this(imageSupplier, width, height, DEFAULT_SCALING_MODE);
    }

    public DerivableImageIcon(final Supplier<Image> imageSupplier, final int width, final int height,
                              final int scalingMode) {
        this.width = width;
        this.height = height;
        this.scalingMode = scalingMode;
        this.imageSupplier = imageSupplier;
    }

    /**
     * Create a new derivable image icon.
     *
     * @param icon the source image icon.
     */
    public DerivableImageIcon(final ImageIcon icon) {
        this(icon, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon with specified scalingMode.
     *
     * @param icon        the source icon.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final ImageIcon icon, final int scalingMode) {
        this(icon != null ? icon.getImage() : null, scalingMode);
    }

    /**
     * Create a new derivable image icon with specified scalingMode.
     *
     * @param icon   the source icon.
     * @param width  the icon width.
     * @param height the icon height.
     */
    public DerivableImageIcon(final ImageIcon icon, final int width, final int height) {
        this(icon, width, height, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon with specified scalingMode. If the source image doesnt match the specified
     * dimensions it will be scaled accordingly.
     *
     * @param icon        the source icon.
     * @param width       the icon width.
     * @param height      the icon height.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final ImageIcon icon, final int width, final int height, final int scalingMode) {
        this(icon != null ? icon.getImage() : null, width, height, scalingMode);
        if (icon != null) setDescription(icon.getDescription());
    }

    /**
     * Create a new derivable image.
     *
     * @param img the source image.
     */
    public DerivableImageIcon(final Image img) {
        this(img, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon with specified scalingMode.
     *
     * @param img         the source image.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final Image img, final int scalingMode) {
        this(img, img != null ? img.getWidth(null) : 0, img != null ? img.getHeight(null) : 0, scalingMode);
    }

    /**
     * Create a new derivable image icon with default scalingMode. If the source image doesnt match the specified
     * dimensions it will be scaled accordingly.
     *
     * @param img    the source image.
     * @param width  the icon width.
     * @param height the icon height.
     */
    public DerivableImageIcon(final Image img, final int width, final int height) {
        this(img, width, height, DEFAULT_SCALING_MODE);
    }

    /**
     * Create a new derivable image icon with specified scalingMode. If the source image doesnt match the specified
     * dimensions it will be scaled accordingly.
     *
     * @param img         the source image.
     * @param width       the icon width.
     * @param height      the icon height.
     * @param scalingMode One of {@link Image#SCALE_DEFAULT}, {@link Image#SCALE_FAST}, {@link Image#SCALE_REPLICATE},
     *                    {@link Image#SCALE_AREA_AVERAGING}, {@link Image#SCALE_SMOOTH}
     */
    public DerivableImageIcon(final Image img, final int width, final int height, final int scalingMode) {
        this.width = width;
        this.height = height;
        this.scalingMode = scalingMode;
        this.original = img;
        if (img != null && img.getWidth(null) == width && img.getHeight(null) == height) {
            this.img = img;
        }
    }

    protected DerivableImageIcon(final DerivableImageIcon parent, final int width, final int height) {
        this.width = width;
        this.height = height;
        this.scalingMode = parent.scalingMode;
        this.original = parent.original;
        this.description = parent.description;
        this.imageSupplier = parent.imageSupplier;
        if (this.original != null
            && this.original.getWidth(null) == width
            && this.original.getHeight(null) == height) {
            this.img = this.original;
        } else if (parent.img != null
                   && parent.img.getWidth(null) == width
                   && parent.img.getHeight(null) == height) {
            this.img = parent.img;
        }
    }

    @Override
    public DerivableImageIcon derive(final int width, final int height) {
        return new DerivableImageIcon(this, width, height);
    }

    protected void ensureOriginalLoaded() {
        if (original == null && imageSupplier != null) {
            original = imageSupplier.get();
            imageSupplier = null;
        }
    }

    protected void ensureLoaded() {
        ensureOriginalLoaded();
        if (original != null && (width < 0 || height < 0)) {
            if (width < 0) width = original.getWidth(null);
            if (height < 0) height = original.getHeight(null);
        }
        if (img == null && original != null && width > 0 && height > 0) {
            if (original.getWidth(null) != width || original.getHeight(null) != height) {
                img = original.getScaledInstance(width, height, scalingMode);
            } else {
                img = original;
            }
        }
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureLoaded();
        if (img != null) {
            g.drawImage(img, x, y, width, height, null);
        }
    }

    /**
     * Get the underlying {@link Image}.
     *
     * @return the image.
     */
    public Image getImage() {
        ensureLoaded();
        return img;
    }

    @Override
    public int getIconWidth() {
        if (width < 0) ensureLoaded();
        return width;
    }

    @Override
    public int getIconHeight() {
        if (height < 0) ensureLoaded();
        return height;
    }

    @Override
    public AccessibleContext getAccessibleContext() {
        if (accessibleContext == null) {
            accessibleContext = new AccessibleDerivableImageIcon(this);
        }
        return accessibleContext;
    }

    /**
     * Gets the description of the image. This is meant to be a brief
     * textual description of the object. For example, it might be
     * presented to a blind user to give an indication of the purpose
     * of the image.
     * The description may be null.
     *
     * @return a brief textual description of the image
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description of the image. This is meant to be a brief
     * textual description of the object. For example, it might be
     * presented to a blind user to give an indication of the purpose
     * of the image.
     *
     * @param description a brief textual description of the image
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    protected static class AccessibleDerivableImageIcon extends AccessibleContext implements AccessibleIcon {

        private final DerivableImageIcon icon;

        public AccessibleDerivableImageIcon(final DerivableImageIcon icon) {
            this.icon = icon;
        }

        @Override
        public AccessibleRole getAccessibleRole() {
            return AccessibleRole.ICON;
        }

        @Override
        public AccessibleStateSet getAccessibleStateSet() {
            return null;
        }

        @Override
        public int getAccessibleIndexInParent() {
            return -1;
        }

        @Override
        public int getAccessibleChildrenCount() {
            return 0;
        }

        @Override
        public Accessible getAccessibleChild(final int i) {
            return null;
        }

        @Override
        public Locale getLocale() throws IllegalComponentStateException {
            return null;
        }

        @Override
        public String getAccessibleIconDescription() {
            return icon.getDescription();
        }

        @Override
        public void setAccessibleIconDescription(final String description) {
            icon.setDescription(description);
        }

        @Override
        public int getAccessibleIconWidth() {
            return icon.getIconWidth();
        }

        @Override
        public int getAccessibleIconHeight() {
            return icon.getIconHeight();
        }
    }
}
