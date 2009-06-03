package com.rontologist.solium.romannumerals.server.numeral.roman;

public class XD extends AbstractRomanNumeral {
    private static final XD INSTANCE = new XD();
    private static final int VALUE = 490;

    private XD() { }

    public static XD getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}