package com.weis.darklaf.color;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class DarkColorModel {

    private final String prefix;
    private final String[] labels;

    @Contract(pure = true)
    public DarkColorModel() {
        this("rgb", "Red", "Green", "Blue");
    }

    @Contract(pure = true)
    public DarkColorModel(final String name, final String... labels) {
        this.prefix = "ColorChooser." + name;
        this.labels = labels;
    }

    public int getCount() {
        return this.labels.length;
    }

    public int getMinimum(final int index) {
        return 0;
    }

    public int getMaximum(final int index) {
        return 255;
    }

    public int getDefault(final int index) {
        return 0;
    }

    public final String getText(@NotNull final Component component, final String suffix) {
        return UIManager.getString(this.prefix + suffix + "Text", component.getLocale());
    }

    @Override
    public String toString() {
        return "RGB";
    }

    public int getValueCount() {
        return 3;
    }

    public char[] getLabelDescriptorsBefore() {
        return new char[]{'R', 'G', 'B'};
    }

    public char[] getLabelDescriptorsAfter() {
        return new char[]{Character.MIN_VALUE, Character.MIN_VALUE, Character.MIN_VALUE, Character.MIN_VALUE};
    }

    public int[] getValuesFromColor(@NotNull final Color color) {
        return new int[]{color.getRed(), color.getGreen(), color.getBlue()};
    }

    public Color getColorFromValues(@NotNull final int[] values) {
        return new Color(values[0], values[1], values[2]);
    }
}
