/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pms.gui;

import javax.swing.filechooser.FileFilter;
import java.io.File;

/**
 *
 * @author kelseycrawford
 */
public class BmpFileFilter extends FileFilter {

    public boolean accept(File file) {
        if (file.getName().toLowerCase().endsWith(".bmp")) {
            return true;
        }
        if (file.isDirectory()) {
            return true;
        }
        return false;
    }

    public String getDescription() {

        return "bmp files";

    }
}
