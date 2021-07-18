/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.core.theme;

import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JLabel;

import com.github.weisj.darklaf.graphics.ThemedColor;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.properties.icons.SolidColorIcon;
import com.github.weisj.darklaf.ui.ComponentDemo;

public class ColorPaletteDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ColorPaletteDemo());
    }

    @Override
    public JComponent createComponent() {
        JComponent comp = Box.createVerticalBox();
        comp.add(Box.createVerticalStrut(5));

        String[] names = {
                "palette.yellow",
                "palette.orange",
                "palette.red",
                "palette.pink",
                "palette.purple",
                "palette.indigo",
                "palette.blue",
                "palette.teal",
                "palette.cyan",
                "palette.green",
                "palette.lime",
                "palette.forest",
                "palette.brown",
                "palette.gray",
        };

        {
            JComponent p = Box.createVerticalBox();
            p.setBorder(LayoutHelper.createEmptyContainerBorder());

            for (String name : names) {
                p.add(Box.createVerticalStrut(5));
                p.add(new JLabel(name, new SolidColorIcon(new ThemedColor(name), 32, 32), JLabel.LEFT));
            }

            comp.add(p);
            comp.add(Box.createVerticalStrut(5));
        }

        return comp;
    }

    @Override
    public String getTitle() {
        return "Color Palette";
    }
}
