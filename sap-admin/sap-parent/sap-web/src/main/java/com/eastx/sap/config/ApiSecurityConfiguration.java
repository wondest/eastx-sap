package com.eastx.sap.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

/**
 * @ClassName ApiSecurityConfiguration
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/3 0:04
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
public class ApiSecurityConfiguration extends WebSecurityConfigurerAdapter {

    //安全拦截机制（最重要）
    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.csrf().disable()
                .authorizeRequests()
                .antMatchers("/demo/**").permitAll()
                .antMatchers("/sec/**").hasRole("USER")
                .anyRequest().authenticated()
                .and()
                .httpBasic();
    }

    //内存用户
    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.inMemoryAuthentication()
                .withUser("admin").password("{noop}123").roles("ADMIN","USER")
                .and()
                .withUser("csa").password("{noop}123").roles("USER");
    }

    //过滤静态资源
    @Override
    public void configure(WebSecurity web) throws Exception {
        //web.ignoring().antMatchers("/demo/**");
    }
}
