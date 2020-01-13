import com.github.weisj.darklaf.LafManager;
import com.kitfox.svg.Defs;
import com.kitfox.svg.LinearGradient;
import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElement;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGRoot;
import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.animation.AnimationElement;
import com.kitfox.svg.app.beans.SVGPanel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.net.URISyntaxException;
import java.util.List;

public class SVGTest {

    public static void main(final String[] args) throws URISyntaxException, SVGElementException {
        LafManager.install();
        JFrame frame = new JFrame();
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        var svgPanel = new SVGPanel() {
        };
        frame.setContentPane(svgPanel);
        svgPanel.setAntiAlias(true);
        svgPanel.setScaleToFit(true);
        svgPanel.setSvgURI(SVGTest.class.getClassLoader()
                                        .getResource("com/github/weisj/darklaf/icons/control/checkBox.svg")
                                        .toURI());
        SVGUniverse svgUniverse = svgPanel.getSvgUniverse();
        SVGDiagram diagram = svgUniverse.getDiagram(svgPanel.getSvgURI());
        loadColors(diagram);

        frame.setSize(500, 500);
        frame.setVisible(true);
    }

    private static void loadColors(final SVGDiagram diagram) throws SVGElementException {
        SVGRoot root = diagram.getRoot();
        SVGElement defs = diagram.getElement("colors");
        List children = defs.getChildren(null);
        root.removeChild(defs);

        Defs themedDefs = new Defs();
        themedDefs.addAttribute("id", AnimationElement.AT_XML, "colors");
        root.loaderAddChild(null, themedDefs);

        for (Object child : children) {
            if (child instanceof LinearGradient) {
                String id = ((LinearGradient) child).getId();
                Color c = UIManager.getColor(id);
                themedDefs.loaderAddChild(null, createColor(c, id));
            }
        }
    }

    @NotNull
    private static LinearGradient createColor(final Color c, final String name) throws SVGElementException {
        LinearGradient grad = new LinearGradient();
        grad.addAttribute("id", AnimationElement.AT_XML, name);
        grad.setStops(new Color[]{c, c}, new float[]{0.0f, 1.0f});
        return grad;
    }
}
