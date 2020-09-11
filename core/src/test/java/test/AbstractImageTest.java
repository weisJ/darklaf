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
package test;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import javax.imageio.ImageIO;

import com.github.weisj.darklaf.util.ImageUtil;

public abstract class AbstractImageTest {
    private static final int SCALING_FACTOR = 3;
    protected final static String WORKING_DIR = "image_test";
    private final String workingDir;

    public AbstractImageTest(final String dir) {
        this.workingDir = dir != null ? WORKING_DIR + "/" + dir : WORKING_DIR;
    }

    protected String getWorkingDirectory() {
        return workingDir;
    }

    protected void createFolder(final String folder) throws IOException {
        Files.createDirectories(new File(getWorkingDirectory() + "/" + folder).toPath());
    }

    protected BufferedImage saveScreenShot(final String name, final Component c) {
        return saveScreenShot(name, c, SCALING_FACTOR);
    }

    protected String getPath(final String name) {
        return getWorkingDirectory() + "/" + name;
    }

    protected BufferedImage saveScreenShot(final String name, final Component c, final double scalingFactor) {
        try {
            File file = new File(name + ".png");
            file.getParentFile().mkdirs();
            Rectangle rect = new Rectangle(0, 0, c.getWidth(), c.getHeight());
            BufferedImage image = ImageUtil.scaledImageFromComponent(c, rect, scalingFactor, scalingFactor, false);
            ImageIO.write(image, "png", file);
            return image;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
