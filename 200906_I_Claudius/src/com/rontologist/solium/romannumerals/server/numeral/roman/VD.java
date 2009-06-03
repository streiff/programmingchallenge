package com.rontologist.solium.romannumerals.server.numeral.roman;

public class VD extends AbstractRomanNumeral {
    private static final VD INSTANCE = new VD();
    private static final int VALUE = 495;

    private VD() { }

    public static VD getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}