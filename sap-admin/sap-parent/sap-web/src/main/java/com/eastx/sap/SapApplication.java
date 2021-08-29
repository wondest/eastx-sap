package com.eastx.sap;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.scheduling.annotation.EnableAsync;

/**
 * @author Tender
 */
@SpringBootApplication
@EnableAsync
@EnableJpaRepositories(basePackages = { "com.eastx.sap.data.dao" })
@EntityScan("com.eastx.sap.data.entity")
public class SapApplication {
	public static void main(String[] args) {
		SpringApplication.run(SapApplication.class, args);
	}
}
