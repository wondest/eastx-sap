import React from 'react';
import * as echarts from 'echarts';
import { PageContainer } from '@ant-design/pro-layout';
import { Button, Card, Typography} from 'antd';
import ReactEcharts from 'echarts-for-react';

class Alpha extends React.Component {
    componentDidMount() {
        const chartDom : HTMLDivElement = document.getElementById('main') as HTMLDivElement;
        // 基于准备好的dom，初始化echarts实例
        var myChart = echarts.init(chartDom);
        // 绘制图表
        myChart.setOption(this.getOption());
    }

    getOption = ()=> {
        let option = {
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

        return option;
    }

    render() {
        return (
        <PageContainer>
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
                <ReactEcharts option={this.getOption()} style={{width: 400, height:400}}/>
            </Card>
            
        </PageContainer>
        );
    }
};

export default Alpha;