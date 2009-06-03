package com.rontologist.solium.romannumerals.server.numeral.roman;

public class VL extends AbstractRomanNumeral {
    private static final VL INSTANCE = new VL();
    private static final int VALUE = 45;

    private VL() { }

    public static VL getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}