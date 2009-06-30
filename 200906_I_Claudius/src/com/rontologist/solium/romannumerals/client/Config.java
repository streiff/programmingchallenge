package com.rontologist.solium.romannumerals.client;

import java.io.IOException;
import java.util.Properties;

/** Configuration manager. */
public class Config {
    private Properties properties;

    /** Creates a new config, with settings loading from the config.properties configuration file */
    public Config() {
	properties = new Properties();
	try {
	    properties.load(getClass().getResourceAsStream("config.properties"));
	} catch (IOException e) {
	    throw new RuntimeException(e);
	}
    }

    /** Gets the web host for the number conversion web service */
    public String getHost() {
	return properties.getProperty("host");
    }

    /** Gets the uri for the number conversion web service */
    public String getService() {
	return properties.getProperty("service");
    }
}