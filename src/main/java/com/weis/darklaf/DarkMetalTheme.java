package com.weis.darklaf;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import java.awt.*;

public class DarkMetalTheme extends DefaultMetalTheme {
    public static final ColorUIResource primary1 = new ColorUIResource(53, 56, 58);
    private static final ColorUIResource white = new ColorUIResource(128, 128, 128);
    private static final ColorUIResource darkBlue = new ColorUIResource(0, 44, 63);
    private static final ColorUIResource lightGray = new ColorUIResource(109, 109, 109);
    private static final ColorUIResource primary2 = new ColorUIResource(50, 66, 114);
    private static final ColorUIResource primary3 = new ColorUIResource(53, 69, 91);
    private final ColorUIResource myControlHighlightColor = new ColorUIResource(108, 111, 113);
    private final ColorUIResource myControlDarkShadowColor = new ColorUIResource(39, 42, 44);
    private final ColorUIResource myControlColor = new ColorUIResource(3948353);
    private final ColorUIResource mySeparatorForeground = new ColorUIResource(53, 56, 58);

    public String getName() {
        return "Darcula theme";
    }

    protected ColorUIResource getPrimary1() {
        return primary1;
    }

    protected ColorUIResource getPrimary2() {
        return primary2;
    }

    protected ColorUIResource getPrimary3() {
        return primary3;
    }

    public ColorUIResource getFocusColor() {
        return new ColorUIResource(Color.black);
    }

    public ColorUIResource getControl() {
        return this.myControlColor;
    }

    public ColorUIResource getControlDarkShadow() {
        return this.myControlDarkShadowColor;
    }

    public ColorUIResource getControlHighlight() {
        return this.myControlHighlightColor;
    }

    public ColorUIResource getMenuBackground() {
        return lightGray;
    }

    public ColorUIResource getMenuSelectedBackground() {
        return darkBlue;
    }

    public ColorUIResource getMenuSelectedForeground() {
        return white;
    }

    public ColorUIResource getSeparatorBackground() {
        return this.getControl();
    }

    public ColorUIResource getSeparatorForeground() {
        return this.mySeparatorForeground;
    }

    public ColorUIResource getAcceleratorSelectedForeground() {
        return white;
    }
}

