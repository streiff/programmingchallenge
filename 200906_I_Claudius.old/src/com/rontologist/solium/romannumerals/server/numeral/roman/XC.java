package com.rontologist.solium.romannumerals.server.numeral.roman;

public class XC extends AbstractRomanNumeral {
    private static final XC INSTANCE = new XC();
    private static final int VALUE = 90;

    private XC() { }

    public static XC getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}