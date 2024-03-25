package com.suchtool.niceauth.configuration;

import com.suchtool.niceauth.property.NiceAuthProperty;
import com.suchtool.niceauth.util.NiceAuthUtil;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class NiceAuthConfiguration {
    @Bean(name = "com.suchtool.niceauth.niceAuthProperty")
    @ConfigurationProperties(prefix = "suchtool.niceauth")
    public NiceAuthProperty niceLogProperty() {
        return new NiceAuthProperty();
    }

    @Bean(name = "com.suchtool.niceauth.niceAuthUtil")
    public NiceAuthUtil niceAuthUtil(NiceAuthProperty niceAuthProperty) {
        return new NiceAuthUtil(niceAuthProperty);
    }
}
