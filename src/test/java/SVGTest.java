import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.app.beans.SVGPanel;

import javax.swing.*;
import java.net.URISyntaxException;

public class SVGTest {

    public static void main(final String[] args) throws URISyntaxException, SVGElementException {
        var frame = new JFrame();
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        var svgPanel = new SVGPanel() {
        };
        frame.setContentPane(svgPanel);
        svgPanel.setAntiAlias(true);
        svgPanel.setScaleToFit(false);
        svgPanel.setSvgURI(SVGTest.class.getClassLoader()
                                        .getResource("com/weis/darklaf/icons/shadow/bottomLeft.svg")
                                        .toURI());
        SVGUniverse svgUniverse = svgPanel.getSvgUniverse();
        SVGDiagram diagram = svgUniverse.getDiagram(svgPanel.getSvgURI());
//        var root = diagram.getRoot();
//        var defs = diagram.getElement("defs");
//
//        var grad = new LinearGradient();
//        grad.addAttribute("id", AnimationElement.AT_XML, "test");
//        grad.setStops(new Color[]{Color.RED, Color.RED}, new float[]{0.0f, 1.0f});
//
//        var defs2 = new Defs();
//        defs2.addAttribute("id", AnimationElement.AT_XML, "defs");
//
//        root.removeChild(defs);
//
//        root.loaderAddChild(null, defs2);
//        defs2.loaderAddChild(null, grad);

        frame.setSize(100, 100);
        frame.pack();
        frame.setVisible(true);
    }
}
