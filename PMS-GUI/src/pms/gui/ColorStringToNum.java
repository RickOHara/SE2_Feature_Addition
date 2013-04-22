/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pms.gui;

/**
 *
 * @author kelseycrawford
 */
public class ColorStringToNum {

    String rgbValues;
    String bgrValues;
    String returnVal;
    String[] redValue;
    String[] greenValue;
    String[] blueValue;

    public String colorToNum(String color, int x) {

        String[] items = color.split("=");


        for (int j = 0; j < items.length; j++) {

            if (j == 0) {
                j++;
            }
            if (j == 1) {

                redValue = items[j].split(",g");

            }
            if (j == 2) {

                greenValue = items[j].split(",b");

            }
            if (j == 3) {

                blueValue = items[j].split("]");

            }

        }



        rgbValues = redValue[0].toString() + " " + greenValue[0].toString()
                + " " + blueValue[0].toString();

        bgrValues = blueValue[0].toString() + " " + greenValue[0].toString()
                + " " + redValue[0].toString();

        switch (x) {

            case 1:
                returnVal = rgbValues;
                break;

            case 2:
                returnVal = bgrValues;
        }

        return returnVal;

    }
}
