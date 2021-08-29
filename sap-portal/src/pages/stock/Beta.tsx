import React from 'react';
import * as echarts from 'echarts';
import { PageContainer } from '@ant-design/pro-layout';
import { Button, Card, Typography, Space} from 'antd';
import ReactEcharts from 'echarts-for-react';
import { useEffect,useState } from 'react';
import { DownloadOutlined } from '@ant-design/icons';

const Beta: React.FC = () => {

    const [count, setCount] = useState(0);

    const option1 = {
        xAxis: {
            data: ['2017-10-24', '2017-10-25', '2017-10-26', '2017-10-27']
        },
        yAxis: {},
        series: [{
            type: 'k',
            data: [
                [20, 34, 10,38],
                [40, 35, 30, 50],
                [31, 38, 33, 44],
                [38, 15, 5, 42]
            ]
        }]
    };

    const option2 = {
        xAxis: {
            data: ['2017-10-24', '2017-10-25', '2017-10-26', '2017-10-27']
        },
        yAxis: {},
        series: [{
            type: 'k',
            data: [
                [20, 34, 10,38],
                [40, 35, 30, 50],
                [31, 38, 33, 44],
                [38, 15, 5, 42]
            ]
        }]
    };

    useEffect(() => {
        const chartDom : HTMLDivElement = document.getElementById('main') as HTMLDivElement;
        // 基于准备好的dom，初始化echarts实例
        var myChart = echarts.init(chartDom);
        // 绘制图表
        myChart.setOption(option1);
    },[]);

    
    useEffect(() => {
        let option1 = {
            xAxis: {
                data: ['2017-10-24', '2017-10-25', '2017-10-26', '2017-10-27']
            },
            yAxis: {},
            series: [{
                type: 'k',
                data: [
                    [20, 34, 10,38],
                    [40, 35, 30, 50],
                    [31, 38, 33, 44],
                    [38, 15, 5, 42]
                ]
            }]
        };
    },[count]);

    return (
        <PageContainer>
            <Card>
              <Space>
                <Button type="primary" icon={<DownloadOutlined />} onClick={()=>{setCount(count+1)}}>Next Option</Button>
              </Space>
            </Card>
            <Card>
                <Typography.Title level={2} style={{ textAlign: 'center' }}>
                    Hello Echarts！
                </Typography.Title>
                <div id="main" style={{ width: 400, height: 400 }}></div>
            </Card>
            <Card>
                <Typography.Title level={2} style={{ textAlign: 'center' }}>
                    Hello ReactEcharts！
                </Typography.Title>
                <ReactEcharts option={option1} style={{width: 400, height:400}}/>
            </Card>
        </PageContainer>
    );
};

export default Beta;