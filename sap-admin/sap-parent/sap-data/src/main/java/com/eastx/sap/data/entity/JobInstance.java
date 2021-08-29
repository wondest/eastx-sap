package com.eastx.sap.data.entity;

import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

/**
 * @ClassName com.eastx.spa.data.Job
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/6 23:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Entity
@Table(name="job_instance")
@Data
public class JobInstance {
    @Id
    @GeneratedValue(strategy= GenerationType.IDENTITY)
    Long id;

    @Column(name="name", nullable = false, length = 64)
    String name;

    @Column(name="biz_key", length = 100)
    String bizKey;

    @Column(name="biz_date", nullable = false, length = 8)
    String bizDate;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name="job_id")
    JobBase job;

    @Column(name="last_updated")
    Timestamp lastUpdated;
}
