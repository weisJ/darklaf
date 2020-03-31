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
 */
package com.github.weisj.darklaf.theme.info;

import javax.swing.*;

public class FontSizeRule {
    private static final FontSizeRule DEFAULT = new FontSizeRule(AdjustmentType.NO_ADJUSTMENT, 0, 1f);

    private final AdjustmentType type;
    private final FontSizePreset preset;
    private final float relativeAdjustment;
    private final int absoluteAdjustment;

    protected FontSizeRule(final FontSizePreset preset) {
        this.preset = preset;
        type = preset.getType();
        relativeAdjustment = 1f;
        absoluteAdjustment = 0;
    }

    protected FontSizeRule(final AdjustmentType type,
                           final int absoluteAdjustment, final float relativeAdjustment) {
        this.type = type;
        this.absoluteAdjustment = absoluteAdjustment;
        this.relativeAdjustment = relativeAdjustment;
        preset = null;
    }

    public static FontSizeRule getDefault() {
        return DEFAULT;
    }

    public static FontSizeRule fromPreset(final FontSizePreset preset) {
        return new FontSizeRule(preset);
    }

    public static FontSizeRule absoluteAdjustment(final int adjustment) {
        return new FontSizeRule(AdjustmentType.ABSOLUTE_ADJUSTMENT, adjustment, 1f);
    }

    public static FontSizeRule relativeAdjustment(final float percent) {
        return new FontSizeRule(AdjustmentType.RELATIVE_ADJUSTMENT, 0, percent);
    }

    public float adjustFontSize(final float size, final UIDefaults defaults) {
        if (preset != null) return preset.adjustFontSize(size, defaults);
        return type.adjustSize(size, absoluteAdjustment, relativeAdjustment);
    }

    public AdjustmentType getType() {
        if (preset == null) {
            // Prevent unnecessary mapping of font.
            if (type == AdjustmentType.ABSOLUTE_ADJUSTMENT && absoluteAdjustment == 0) {
                return AdjustmentType.NO_ADJUSTMENT;
            }
            if (type == AdjustmentType.RELATIVE_ADJUSTMENT && relativeAdjustment == 1f) {
                return AdjustmentType.NO_ADJUSTMENT;
            }
        }
        return type;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("FontSizeRule{type=").append(type);
        if (preset != null) {
            builder.append(", preset=").append(preset);
        } else {
            switch (type) {
                case ABSOLUTE_ADJUSTMENT:
                    builder.append(", absoluteAdjustment=").append(absoluteAdjustment);
                    break;
                case RELATIVE_ADJUSTMENT:
                    builder.append(", relativeAdjustment=").append(relativeAdjustment);
                    break;
                case NO_ADJUSTMENT:
                default:
                    break;
            }
        }
        builder.append("}");
        return builder.toString();
    }

    public enum AdjustmentType {
        NO_ADJUSTMENT {
            @Override
            public float adjustSize(final float size, final int absolute, final float relative) {
                return size;
            }
        },
        ABSOLUTE_ADJUSTMENT {
            @Override
            public float adjustSize(final float size, final int absolute, final float relative) {
                return size + absolute;
            }
        },
        RELATIVE_ADJUSTMENT {
            @Override
            public float adjustSize(final float size, final int absolute, final float relative) {
                return size * relative;
            }
        };

        abstract public float adjustSize(final float size, final int absolute, final float relative);
    }

}
