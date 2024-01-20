# The_Path - Masters_Journey

<h6> Don't speak Portuguese?! <a href = "https://github.com/gosvnavarro/The_Path_Masters_Journey/blob/main/README-en.md">Click here</a> to view this page in English.</h6>

Coleção de scripts usados para a manipulação e análise de dados genéticos e ambientais no desenvolvimento do meu projeto de mestrado realizado entre 2019 a 2021. Abaixo estão descritos os scripts principais e algumas informações relevantes:
<br>
<ol>
    <li><b>prs_&_derivatives.txt</b>
    <ol>
        <li>PRS = Escore Poligênico de Risco.</li>
        <li>PRSice, versão 2.</li>
        <li>Realizado pela linha de comando (terminal/prompt).</li>
    </ol>
    </li>
				<br>
    <li><b>pers_score.R</b>
				<ol>
        <li>PERS = Escore dos Fatores Ambientais de Risco.</li>
        <li>É importante que os dados (tabela de entrada) utilizados para o cálculo estejam na forma binária (SIM/NÃO) ou com as respectivas odds ratios (se aplicável).</li>
        <li>Antes de enviar o arquivo para o ambiente R, é recomendável salvá-lo no formato ‘.csv’.</li>
								<li>A ordem aqui descrita das variáveis ambientais segue a ordem desenvolvida no artigo de referência ('The "polyenviromic risk score": Aggregating environmental risk factors predicts conversion to psychosis in familial high-risk subjects', Padmanabhan, JL, et al. 2016).</li>
    </ol>
				</li>
				<br>
    <li><b>statistical_analysis.R</b>
    <ol>
        <li>Análises estatísticas (correlação, regressão, etc.) em R.</li>
        <li>Antes de enviar o arquivo para o ambiente R, é recomendável salvá-lo no formato ‘.csv’.</li>
    </ol>
    </li>
</ol>
