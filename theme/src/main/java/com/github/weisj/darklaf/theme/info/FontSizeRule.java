/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.theme.info;

public class FontSizeRule {
    private static final FontSizeRule DEFAULT = new FontSizeRule(AdjustmentType.NO_ADJUSTMENT, 1f);

    private final AdjustmentType type;
    private final FontSizePreset preset;
    private final float relativeAdjustment;

    protected FontSizeRule(final FontSizePreset preset) {
        this.preset = preset;
        type = preset.getType();
        relativeAdjustment = 1f;
    }

    protected FontSizeRule(final AdjustmentType type, final float relativeAdjustment) {
        this.type = type;
        this.relativeAdjustment = relativeAdjustment;
        preset = null;
    }

    public FontSizePreset getPreset() {
        return preset;
    }

    public static FontSizeRule getDefault() {
        return DEFAULT;
    }

    public static FontSizeRule fromPreset(final FontSizePreset preset) {
        return new FontSizeRule(preset);
    }

    public static FontSizeRule relativeAdjustment(final float percent) {
        return new FontSizeRule(AdjustmentType.RELATIVE_ADJUSTMENT, percent);
    }

    public static FontSizeRule relativeAdjustment(final int percent) {
        return new FontSizeRule(AdjustmentType.RELATIVE_ADJUSTMENT, percent / 100f);
    }

    public float adjustFontSize(final float size) {
        if (preset != null) return preset.adjustFontSize(size);
        return type.adjustSize(size, relativeAdjustment);
    }

    public AdjustmentType getType() {
        if (preset == null) {
            // Prevent unnecessary mapping of font.
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

    public int getPercentage() {
        if (preset != null) return preset.getPercentage();
        return Math.round(relativeAdjustment * 100);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FontSizeRule that = (FontSizeRule) o;
        return Float.compare(that.getPercentage(), getPercentage()) == 0;
    }

    @Override
    public int hashCode() {
        int result = type != null ? type.hashCode() : 0;
        result = 31 * result + (preset != null ? preset.hashCode() : 0);
        result = 31 * result + (relativeAdjustment != +0.0f ? Float.floatToIntBits(relativeAdjustment) : 0);
        return result;
    }

    public enum AdjustmentType {
        NO_ADJUSTMENT {
            @Override
            public float adjustSize(final float size, final float relative) {
                return size;
            }
        },
        RELATIVE_ADJUSTMENT {
            @Override
            public float adjustSize(final float size, final float relative) {
                return size * relative;
            }
        };

        public abstract float adjustSize(final float size, final float relative);
    }
}
