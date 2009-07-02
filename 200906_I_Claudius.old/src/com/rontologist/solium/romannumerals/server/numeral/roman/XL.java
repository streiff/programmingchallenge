package com.rontologist.solium.romannumerals.server.numeral.roman;

public class XL extends AbstractRomanNumeral {
    private static final XL INSTANCE = new XL();
    private static final int VALUE = 40;

    private XL() { }

    public static XL getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}