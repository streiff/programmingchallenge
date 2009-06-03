package com.rontologist.solium.romannumerals.server.numeral.roman;

public class IM extends AbstractRomanNumeral {
    private static final IM INSTANCE = new IM();
    private static final int VALUE = 999;

    private IM() { }

    public static IM getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}