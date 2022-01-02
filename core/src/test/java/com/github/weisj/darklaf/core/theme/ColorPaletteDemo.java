/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.core.theme;

import java.awt.Color;
import java.util.Arrays;
import java.util.List;

import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JLabel;

import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.properties.color.DynamicColor;
import com.github.weisj.darklaf.properties.icons.SolidColorIcon;
import com.github.weisj.darklaf.theme.ColorPalette;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class ColorPaletteDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ColorPaletteDemo());
    }

    @Override
    public JComponent createComponent() {
        JComponent comp = Box.createVerticalBox();
        comp.add(Box.createVerticalStrut(5));

        {
            JComponent p = Box.createVerticalBox();
            p.setBorder(LayoutHelper.createEmptyContainerBorder());

            List<Color> palette = Arrays.asList(
                    ColorPalette.YELLOW,
                    ColorPalette.ORANGE,
                    ColorPalette.RED,
                    ColorPalette.PINK,
                    ColorPalette.PURPLE,
                    ColorPalette.INDIGO,
                    ColorPalette.BLUE,
                    ColorPalette.TEAL,
                    ColorPalette.CYAN,
                    ColorPalette.GREEN,
                    ColorPalette.LIME,
                    ColorPalette.FOREST,
                    ColorPalette.BROWN,
                    ColorPalette.GRAY);

            for (Color color : palette) {
                p.add(Box.createVerticalStrut(5));
                p.add(new JLabel(((DynamicColor) color).getKey(), new SolidColorIcon(color, 32, 32), JLabel.LEFT));
            }

            comp.add(p);
            comp.add(Box.createVerticalStrut(5));
        }

        return comp;
    }

    @Override
    public String getName() {
        return "Color Palette";
    }
}
