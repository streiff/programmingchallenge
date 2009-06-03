package com.rontologist.solium.romannumerals.server.numeral.roman;

public class VC extends AbstractRomanNumeral {
    private static final VC INSTANCE = new VC();
    private static final int VALUE = 95;

    private VC() { }

    public static VC getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}