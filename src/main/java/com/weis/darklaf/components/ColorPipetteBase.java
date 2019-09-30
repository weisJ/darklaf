package com.weis.darklaf.components;

import com.bulenkov.iconloader.util.SystemInfo;
import com.weis.darklaf.platform.windows.WindowsFrameUtil;
import com.weis.darklaf.ui.colorchooser.ColorListener;
import com.weis.darklaf.ui.colorchooser.ColorPipette;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.plaf.basic.BasicRootPaneUI;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;

public abstract class ColorPipetteBase implements ColorPipette, AWTEventListener {
    protected final JComponent parent;
    protected final Robot robot;
    private final ColorListener colorListener;
    private Runnable closeAction;
    private JWindow pickerWindow;
    private boolean keyDown;
    private int downKeyCode;

    private Color currentColor;
    private Color initialColor;

    public ColorPipetteBase(@NotNull final JComponent parent, @NotNull final ColorListener colorListener) {
        this.parent = parent;
        this.colorListener = colorListener;
        robot = createRobot();
    }

    @Nullable
    private static Robot createRobot() {
        try {
            return new Robot();
        } catch (AWTException e) {
            return null;
        }
    }

    public boolean isKeyDown() {
        return keyDown;
    }

    public int getPressedKeyCode() {
        return downKeyCode;
    }

    public void setCloseAction(final Runnable closeAction) {
        this.closeAction = closeAction;
    }

    @Override
    public void pickAndClose() {
        PointerInfo pointerInfo = MouseInfo.getPointerInfo();
        Color pixelColor = getPixelColor(pointerInfo.getLocation());
        cancelPipette();
        notifyListener(pixelColor);
        setInitialColor(pixelColor);
    }

    protected Color getPixelColor(@NotNull final Point location) {
        return robot.getPixelColor(location.x, location.y);
    }

    @Nullable
    protected Color getInitialColor() {
        return initialColor;
    }

    @Override
    public void setInitialColor(@Nullable final Color initialColor) {
        this.initialColor = initialColor;
        setColor(initialColor);
    }

    @Nullable
    @Override
    public Color getColor() {
        return currentColor;
    }

    protected void setColor(@Nullable final Color color) {
        currentColor = color;
    }

    @Override
    public Window show() {
        Window picker = getOrCreatePickerWindow();
        Toolkit.getDefaultToolkit().addAWTEventListener(this,
                                                        AWTEvent.MOUSE_MOTION_EVENT_MASK
                                                        | AWTEvent.MOUSE_EVENT_MASK
                                                        | AWTEvent.KEY_EVENT_MASK);
        updateLocation();
        picker.setVisible(true);
        return picker;
    }

    @Nullable
    protected Point updateLocation() {
        PointerInfo pointerInfo = MouseInfo.getPointerInfo();
        if (pointerInfo == null) return null;

        Point mouseLocation = pointerInfo.getLocation();
        Window pickerWindow = getPickerWindow();
        if (pickerWindow != null && mouseLocation != null) {
            pickerWindow.setLocation(adjustPickerLocation(mouseLocation, pickerWindow));
        }
        return mouseLocation;
    }

    protected Point adjustPickerLocation(@NotNull final Point mouseLocation, @NotNull final Window pickerWindow) {
        return new Point(mouseLocation.x - pickerWindow.getWidth() / 2,
                         mouseLocation.y - pickerWindow.getHeight() / 2);
    }

    @Nullable
    protected Window getPickerWindow() {
        return pickerWindow;
    }

    @NotNull
    protected Window getOrCreatePickerWindow() {
        if (pickerWindow == null) {
            Window owner = SwingUtilities.getWindowAncestor(parent);
            pickerWindow = createPickerWindow(owner);
            pickerWindow.setName("DarkLafPickerDialog");
            JRootPane rootPane = pickerWindow.getRootPane();
            rootPane.putClientProperty("Window.shadow", Boolean.FALSE);
        }
        return pickerWindow;
    }

    @Override
    public void eventDispatched(@NotNull final AWTEvent event) {
        if (pickerWindow == null || !pickerWindow.isVisible()) return;
        switch (event.getID()) {
            case MouseEvent.MOUSE_PRESSED:
                ((MouseEvent) event).consume();
                pickAndClose();
                break;
            case MouseEvent.MOUSE_CLICKED:
                ((MouseEvent) event).consume();
                break;
            case KeyEvent.KEY_PRESSED:
                downKeyCode = ((KeyEvent) event).getKeyCode();
                switch (downKeyCode) {
                    case KeyEvent.VK_ESCAPE:
                        cancelPipette();
                        break;
                    case KeyEvent.VK_ENTER:
                        pickAndClose();
                        break;
                    default:
                        break;
                }
                if (!keyDown) {
                    keyDown = true;
                    updatePipette(true);
                }
                break;
            case KeyEvent.KEY_RELEASED:
                keyDown = false;
                var picker = getPickerWindow();
                if (picker != null) {
                    picker.repaint();
                }
                break;
            default:
                break;
        }
    }

    protected abstract void updatePipette(final boolean force);

    protected void notifyListener(@NotNull final Color c) {
        colorListener.colorChanged(c, this);
    }

    @Override
    public boolean imageUpdate(final Image image, final int i, final int i1,
                               final int i2, final int i3, final int i4) {
        return false;
    }

    @Override
    public void cancelPipette() {
        Window pickerWindow = getPickerWindow();
        if (pickerWindow != null) {
            pickerWindow.setVisible(false);
        }
        Color initialColor = getInitialColor();
        if (initialColor != null) {
            notifyListener(initialColor);
        }
        if (closeAction != null) {
            closeAction.run();
        }
        Toolkit.getDefaultToolkit().removeAWTEventListener(this);
        if (SystemInfo.isWindows) {
            WindowsFrameUtil.User32dll.INSTANCE.ShowCursor(true);
        }
    }

    @Override
    public void dispose() {
        pickerWindow.dispose();
        pickerWindow = null;
        setInitialColor(null);
        setColor(null);
    }

    protected JWindow createPickerWindow(final Window parent) {
        return new PickerWindow(parent);
    }

    protected static class PickerWindow extends JWindow {

        protected PickerWindow(final Window parent) {
            super(parent);
            setBackground(DarkUIUtil.TRANSPARENT_COLOR);
            BufferedImage cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
            Cursor blankCursor = Toolkit.getDefaultToolkit()
                                        .createCustomCursor(cursorImg, new Point(), "BlankCursor");
            setCursor(blankCursor);
        }

        @NotNull
        @Override
        public Cursor getCursor() {
            return super.getCursor();
        }

        @Override
        protected JRootPane createRootPane() {
            return new JRootPane() {
                @Override
                public void updateUI() {
                    setUI(new BasicRootPaneUI());
                }

                @Override
                public int getWindowDecorationStyle() {
                    return NONE;
                }
            };
        }
    }
}
