// Página da apuração: lê dados/apuracao.json (gerado pelo R/apuracao.R a cada ciclo) e redesenha.
// ?replay[=segundos] reproduz a série ponto a ponto (demonstração).
"use strict";

const ARQ = "dados/apuracao.json";
const INTERVALO_MS = 30000;
const BLOCOS = ["PT", "PL", "OUTROS"];
const fmt = (v, d = 1) => (v == null ? "–" : v.toLocaleString("pt-BR", { minimumFractionDigits: d, maximumFractionDigits: d }));
const cor = (b) => getComputedStyle(document.documentElement).getPropertyValue(`--s-${b}`).trim();
const el = (tag, cls, txt) => { const e = document.createElement(tag); if (cls) e.className = cls; if (txt != null) e.textContent = txt; return e; };

let dados = null;
let pontosVisiveis = null;   // no modo replay, recorte da série

// ---------------------------------------------------------------------------------------------------------
async function carregar() {
  try {
    const r = await fetch(`${ARQ}?t=${Date.now()}`, { cache: "no-store" });
    if (!r.ok) throw new Error(r.status);
    dados = await r.json();
    if (pontosVisiveis === null) desenhar(dados.pontos);
  } catch (e) {
    document.getElementById("status").textContent = "Não foi possível carregar os dados agora; nova tentativa em 30 segundos.";
  }
}

function nomes() {
  const m = {};
  for (const c of dados.candidatos) m[c.bloco] = c;
  return m;
}

// ---------------------------------------------------------------------------------------------------------
function desenhar(pontos) {
  const espera = !pontos || !pontos.length;
  for (const id of ["cartoes", "painel", "dados-tabela", "estados"]) { const e = document.getElementById(id); if (e) e.hidden = espera; }
  if (espera) { aguardando(); return; }
  const ult = pontos[pontos.length - 1];
  const comProj = [...pontos].reverse().find((p) => p.projecao);
  cabecalho(ult, comProj);
  cartoes(ult, comProj);
  legenda();
  grafico(pontos);
  tabela(pontos);
  estados();
}

// Antes da primeira seção apurada: título da eleição e a hora da última verificação no TSE.
function aguardando() {
  document.getElementById("eleicao").textContent = `${dados.eleicao} · ${dados.turno}º turno`;
  document.getElementById("aviso").hidden = true;
  const st = document.getElementById("status");
  st.replaceChildren();
  const hora = (dados.atualizado || "").split(" ")[1];
  st.append(el("strong", null, "Aguardando o início da apuração."), " As urnas fecham às 17h (Brasília); os primeiros resultados",
    " saem logo depois e esta página se atualiza sozinha.", hora ? ` Última verificação no TSE: ${hora}.` : "");
}

function cabecalho(ult, comProj) {
  document.getElementById("eleicao").textContent = `${dados.eleicao} · ${dados.turno}º turno`;
  const aviso = document.getElementById("aviso");
  const avisos = {
    replay: "Demonstração: reprodução da apuração de 2022, na ordem real de chegada dos boletins, com os nomes dos candidatos de 2026. Não são resultados de 2026.",
    simulado: "Teste: dados fictícios do simulado do TSE. Não são resultados reais.",
  };
  aviso.hidden = !avisos[dados.ambiente];
  aviso.textContent = avisos[dados.ambiente] || "";
  const st = document.getElementById("status");
  st.replaceChildren();
  st.append(el("strong", null, `${fmt(ult.x, 2)}%`), " do eleitorado apurado · atualizado às ", el("strong", null, ult.t));
  if (!comProj) st.append(el("span", "selo", "projeção imprecisa até 2% apurado"));
  else if (ult.x < 5) st.append(el("span", "selo", "projeção preliminar"));
  const p2 = document.getElementById("p2t");
  p2.replaceChildren();
  if (comProj && comProj.p_2turno != null) {
    const v = comProj.p_2turno;
    const txt = v >= 0.995 ? "> 99%" : v <= 0.005 ? "< 1%" : `${Math.round(100 * v)}%`;
    p2.append("Probabilidade de 2º turno: ", el("strong", null, txt));
  }
}

function cartoes(ult, comProj) {
  const n = nomes();
  const box = document.getElementById("cartoes");
  box.replaceChildren();
  for (const b of BLOCOS) {
    const c = el("article", "cartao");
    const nome = el("div", "cartao-nome");
    const chave = el("span", "chave"); chave.style.background = cor(b);
    nome.append(chave, el("span", null, n[b].nome));
    if (n[b].partido) nome.append(el("span", "cartao-partido", n[b].partido));
    c.append(nome);
    if (comProj) {
      const v = el("div", "cartao-valor", fmt(comProj.projecao[b]));
      v.append(el("span", "pct", "%"));
      c.append(v, el("div", "cartao-rotulo", "projeção do resultado final"));
      const f = comProj.faixa[b];
      c.append(el("div", "cartao-faixa", `faixa de 90%: ${fmt(f[0])} – ${fmt(f[1])}`));
    } else if (dados.projecao_preliminar) {
      const v = el("div", "cartao-valor preliminar");
      v.append(el("span", "num", fmt(dados.projecao_preliminar[b])), el("span", "pct", "%"));
      c.append(v, el("div", "cartao-rotulo", "projeção imprecisa: espere ao menos 2% da apuração"));
    } else {
      const v = el("div", "cartao-valor", fmt(ult.apurado[b]));
      v.append(el("span", "pct", "%"));
      c.append(v, el("div", "cartao-rotulo", "dos votos válidos apurados"));
    }
    c.append(el("div", "cartao-apurado", `apurado agora: ${fmt(ult.apurado[b])}%`));
    box.append(c);
  }
}

function chaveSVG(b, tipo) {
  const ns = "http://www.w3.org/2000/svg";
  const s = document.createElementNS(ns, "svg");
  s.setAttribute("width", "22"); s.setAttribute("height", "10");
  if (tipo === "faixa") {
    const r = document.createElementNS(ns, "rect");
    r.setAttribute("x", "1"); r.setAttribute("y", "1"); r.setAttribute("width", "20"); r.setAttribute("height", "8"); r.setAttribute("rx", "2");
    r.setAttribute("fill", "var(--texto-3)"); r.setAttribute("fill-opacity", ".22"); s.append(r);
  } else {
    const l = document.createElementNS(ns, "line");
    l.setAttribute("x1", "1"); l.setAttribute("x2", "21"); l.setAttribute("y1", "5"); l.setAttribute("y2", "5");
    l.setAttribute("stroke", b ? `var(--s-${b})` : "var(--texto-2)"); l.setAttribute("stroke-width", "2.5"); l.setAttribute("stroke-linecap", "round");
    if (tipo === "proj") l.setAttribute("stroke-dasharray", "0.1 4");
    s.append(l);
  }
  return s;
}

function legenda() {
  const n = nomes();
  const box = document.getElementById("legenda");
  box.replaceChildren();
  for (const b of BLOCOS) { const i = el("span", "leg-item"); i.append(chaveSVG(b, "linha"), n[b].nome); box.append(i); }
  const a = el("span", "leg-item"); a.append(chaveSVG(null, "linha"), "apurado"); box.append(a);
  const p = el("span", "leg-item"); p.append(chaveSVG(null, "proj"), "projeção"); box.append(p);
  const f = el("span", "leg-item"); f.append(chaveSVG(null, "faixa"), "faixa de 90%"); box.append(f);
}

// Um painel de linhas: apurado (cheia), projeção (pontilhada) com faixa, cone do ponto atual até a faixa em 100%.
// Os dois painéis (disputa principal e outros) compartilham o eixo x (% do eleitorado apurado).
function painel(idBox, pontos, blocos, opt) {
  const box = document.getElementById(idBox);
  const W = box.clientWidth || 900;
  const estreito = W < 560;
  const H = opt.altura(estreito);
  const m = { t: 14, r: estreito ? 104 : 150, b: opt.eixoX ? 34 : 8, l: 40 };
  const iw = W - m.l - m.r, ih = H - m.t - m.b;
  const comProj = pontos.filter((p) => p.projecao);
  const ult = pontos[pontos.length - 1];
  const fim = comProj.length ? comProj[comProj.length - 1] : null;

  const vals = [];
  for (const p of pontos) for (const b of blocos) {
    vals.push(p.apurado[b]);
    if (p.faixa) vals.push(p.faixa[b][0], p.faixa[b][1]);
  }
  const passo = opt.passoY;
  let y0 = Math.max(0, Math.floor((d3.min(vals) - 1) / passo) * passo);
  let y1 = Math.min(100, Math.ceil((d3.max(vals) + 1) / passo) * passo);
  if (opt.incluir50 && y1 < 52) y1 = Math.max(y1, 52);
  const x = d3.scaleLinear().domain([0, 100]).range([0, iw]);
  const y = d3.scaleLinear().domain([y0, y1]).range([ih, 0]);

  const svg = d3.create("svg").attr("viewBox", `0 0 ${W} ${H}`).attr("width", W).attr("height", H);
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const ticksY = y.ticks(opt.nTicksY);
  g.append("g").attr("class", "grade").selectAll("line").data(ticksY).join("line")
    .attr("x1", 0).attr("x2", iw).attr("y1", (d) => y(d)).attr("y2", (d) => y(d));
  if (opt.eixoX) g.append("g").attr("class", "eixo").attr("transform", `translate(0,${ih})`)
    .call(d3.axisBottom(x).tickValues([0, 20, 40, 60, 80, 100]).tickFormat((d) => `${d}%`).tickSize(0).tickPadding(10))
    .call((s) => s.select(".domain").remove());
  g.append("g").attr("class", "eixo")
    .call(d3.axisLeft(y).tickValues(ticksY).tickFormat((d) => `${d}%`).tickSize(0).tickPadding(8))
    .call((s) => s.select(".domain").remove());
  if (opt.incluir50 && 50 > y0 && 50 < y1) {
    g.append("line").attr("class", "ref50").attr("x1", 0).attr("x2", iw).attr("y1", y(50)).attr("y2", y(50));
    g.append("text").attr("class", "ref50-rot").attr("x", x(99)).attr("y", y(50) - 6).attr("text-anchor", "end").text("50% dos válidos");
  }

  const linha = (acc) => d3.line().defined((p) => acc(p) != null).x((p) => x(p.x)).y((p) => y(acc(p))).curve(d3.curveMonotoneX);

  // trajetória projetada (só para o ciclo mais recente): do ponto apurado atual até 100%, com a faixa de 90%
  // abrindo ao longo do caminho (zero agora, faixa final em 100%)
  const traj = fim && dados.trajetoria && ult === dados.pontos[dados.pontos.length - 1] ? dados.trajetoria : null;
  for (const b of blocos) {
    const c = cor(b);
    if (fim) {
      let cam;
      if (traj) {
        const x0 = ult.x, v0 = ult.apurado[b], vEnd = traj[traj.length - 1][b];
        cam = [{ x: x0, v: v0 }, ...traj.filter((q) => q.x > x0).map((q) => ({ x: q.x, v: q[b] + (fim.projecao[b] - vEnd) * (q.x - x0) / (100 - x0) }))];
      } else {
        cam = [{ x: ult.x, v: ult.apurado[b] }, { x: 100, v: fim.projecao[b] }];
      }
      const hwFim = (fim.faixa[b][1] - fim.faixa[b][0]) / 2, dc = fim.projecao[b] - (fim.faixa[b][1] + fim.faixa[b][0]) / 2;
      const fr = (q) => (q.x - ult.x) / (100 - ult.x);
      g.append("path").attr("fill", c).attr("fill-opacity", 0.13)
        .attr("d", d3.area().x((q) => x(q.x)).y0((q) => y(q.v - (hwFim + dc) * fr(q))).y1((q) => y(q.v + (hwFim - dc) * fr(q)))
          .curve(d3.curveMonotoneX)(cam));
      g.append("path").attr("d", d3.line().x((q) => x(q.x)).y((q) => y(q.v)).curve(d3.curveMonotoneX)(cam)).attr("fill", "none")
        .attr("stroke", c).attr("stroke-width", 2).attr("stroke-linecap", "round").attr("stroke-dasharray", "0.1 4.5");
    }
    g.append("path").attr("d", linha((p) => p.apurado[b])(pontos)).attr("fill", "none")
      .attr("stroke", c).attr("stroke-width", 2).attr("stroke-linejoin", "round").attr("stroke-linecap", "round");
    g.append("circle").attr("cx", x(ult.x)).attr("cy", y(ult.apurado[b])).attr("r", 4.5)
      .attr("fill", c).attr("stroke", "var(--superficie)").attr("stroke-width", 2);
    if (fim) {
      g.append("line").attr("x1", x(100)).attr("x2", x(100)).attr("y1", y(fim.faixa[b][0])).attr("y2", y(fim.faixa[b][1]))
        .attr("stroke", c).attr("stroke-width", 2).attr("stroke-linecap", "round");
      g.append("circle").attr("cx", x(100)).attr("cy", y(fim.projecao[b])).attr("r", 5)
        .attr("fill", c).attr("stroke", "var(--superficie)").attr("stroke-width", 2);
    }
  }

  // rótulos no fim (em 100% com projeção; senão no ponto atual); separados sem sair do painel
  const n = nomes();
  const rot = blocos.map((b) => ({ b, v: fim ? fim.projecao[b] : ult.apurado[b], f: fim ? fim.faixa[b] : null }));
  rot.sort((a, c) => c.v - a.v);
  const gap = fim ? (estreito ? 30 : 34) : 18;
  let ant = -Infinity;
  for (const r of rot) { r.py = Math.max(y(r.v), ant + gap); ant = r.py; }
  const excesso = ant - (ih - (fim ? 14 : 0));
  if (excesso > 0) for (const r of rot) r.py -= excesso;
  const xr = fim ? x(100) + 12 : x(ult.x) + 10;
  for (const r of rot) {
    if (Math.abs(r.py - y(r.v)) > 2) g.append("line").attr("x1", xr - 8).attr("x2", xr - 2).attr("y1", y(r.v)).attr("y2", r.py)
      .attr("stroke", "var(--texto-3)").attr("stroke-width", 1);
    const t = g.append("text").attr("class", "rot-fim").attr("x", xr).attr("y", r.py + 4);
    t.append("tspan").text(`${n[r.b].nome} ${fmt(r.v)}%`);
    if (r.f) t.append("tspan").attr("class", "sub").attr("x", xr).attr("dy", 15).text(`${fmt(r.f[0])}–${fmt(r.f[1])}`);
  }

  // hover: cruz vertical no ponto mais próximo em x; a dica mostra todos os blocos
  const cruz = g.append("line").attr("class", "cruz").attr("y1", 0).attr("y2", ih).style("display", "none");
  const dica = document.getElementById("dica");
  const idx = d3.bisector((p) => p.x).center;
  g.append("rect").attr("width", iw).attr("height", ih).attr("fill", "transparent").style("cursor", "crosshair")
    .on("pointermove", (ev) => {
      const [mx] = d3.pointer(ev);
      const p = pontos[idx(pontos, x.invert(mx))];
      cruz.attr("x1", x(p.x)).attr("x2", x(p.x)).style("display", null);
      dica.replaceChildren(el("div", "dica-topo", `${p.t} · ${fmt(p.x, 1)}% apurado`));
      for (const b of BLOCOS) {
        const row = el("div", "dica-linha");
        row.append(chaveSVG(b, "linha"), el("span", "n", n[b].nome), el("span", "v", `${fmt(p.apurado[b])}%`));
        if (p.projecao) row.append(el("span", "dica-proj", `projeção ${fmt(p.projecao[b])}% (${fmt(p.faixa[b][0])}–${fmt(p.faixa[b][1])})`));
        dica.append(row);
      }
      dica.hidden = false;
      const bx = dica.getBoundingClientRect();
      let lx = ev.clientX + 16;
      if (lx + bx.width > window.innerWidth - 8) lx = ev.clientX - bx.width - 16;
      const ly = Math.max(8, Math.min(ev.clientY - bx.height / 2, window.innerHeight - bx.height - 8));
      dica.style.left = `${lx}px`; dica.style.top = `${ly}px`;
    })
    .on("pointerleave", () => { cruz.style("display", "none"); dica.hidden = true; });

  box.replaceChildren(svg.node());
}

function grafico(pontos) {
  painel("grafico", pontos, ["PT", "PL"], { altura: (e) => (e ? 300 : 380), eixoX: false, passoY: 2, nTicksY: 6, incluir50: true });
  painel("grafico-outros", pontos, ["OUTROS"], { altura: (e) => (e ? 120 : 140), eixoX: true, passoY: 2, nTicksY: 3, incluir50: false });
}

function tabela(pontos) {
  const n = nomes();
  const t = document.getElementById("tabela");
  t.replaceChildren();
  const cab = el("tr");
  cab.append(el("th", null, "Hora"), el("th", null, "% apurado"));
  for (const b of BLOCOS) cab.append(el("th", null, `${n[b].nome} apurado`));
  for (const b of BLOCOS) cab.append(el("th", null, `${n[b].nome} projeção`));
  const th = el("thead"); th.append(cab); t.append(th);
  const tb = el("tbody");
  for (const p of [...pontos].reverse()) {
    const tr = el("tr");
    tr.append(el("td", null, p.t), el("td", null, fmt(p.x, 1)));
    for (const b of BLOCOS) tr.append(el("td", null, fmt(p.apurado[b])));
    for (const b of BLOCOS) tr.append(el("td", null, p.projecao ? `${fmt(p.projecao[b])} (${fmt(p.faixa[b][0])}–${fmt(p.faixa[b][1])})` : "–"));
    tb.append(tr);
  }
  t.append(tb);
}

// Diferença entre a urna e a pesquisa em cada estado: projeção de cada candidato na UF e, entre parênteses, a
// diferença para a média das pesquisas no estado. A célula ganha a cor do candidato quando ele supera a pesquisa;
// a intensidade cresce de 0,5 a 4 p.p. (4 p.p. ou mais = cor máxima). UF com menos de 2% do seu eleitorado apurado
// aparece fraca e sem cor. Ordem: regiões e, dentro delas, UFs pelo voto no PT no 1º turno de 2022.
const ORDEM_UF = [["PI","NE"],["BA","NE"],["MA","NE"],["CE","NE"],["PE","NE"],["PB","NE"],["SE","NE"],["RN","NE"],["AL","NE"],
  ["PA","N"],["TO","N"],["AM","N"],["AP","N"],["AC","N"],["RO","N"],["RR","N"],["MG","SE"],["SP","SE"],["RJ","SE"],["ES","SE"],
  ["GO","CO"],["MS","CO"],["DF","CO"],["MT","CO"],["RS","S"],["PR","S"],["SC","S"]];
const REGIOES = { NE: "Nordeste", N: "Norte", SE: "Sudeste", CO: "Centro-Oeste", S: "Sul" };
const DIF_MAX = 4;
const tom = (b, d) => {                                   // cor de fundo para quem superou a pesquisa em d p.p.
  if (d < 0.5) return null;
  const c = d3.color(cor(b)); c.opacity = 0.12 + 0.58 * Math.min((d - 0.5) / (DIF_MAX - 0.5), 1);
  return c.formatRgb();
};

function estados() {
  const box = document.getElementById("estados");
  const lista = dados.estados;
  box.hidden = !(lista && lista.length);
  if (box.hidden) return;
  const n = nomes();
  document.getElementById("nota-estados-demo").hidden = dados.ambiente !== "replay";
  const leg = document.getElementById("legenda-estados");
  leg.replaceChildren();
  for (const b of ["PT", "PL"]) {
    const g = el("span", "leg-estados");
    g.append(`${n[b].nome} acima da pesquisa:`);
    for (const d of [1, 2, 3, 4]) { const s = el("span", "amostra", `+${d}`); s.style.background = tom(b, d); g.append(s); }
    leg.append(g);
  }
  const porUf = Object.fromEntries(lista.map((e) => [e.uf, e]));
  const t = document.getElementById("tabela-estados");
  t.replaceChildren();
  const cab = el("tr");
  cab.append(el("th"), el("th", "uf-sigla", "UF"), el("th", "col-cand", n.PT.nome), el("th", "col-cand", n.PL.nome), el("th", "col-barra", "apurado no estado"));
  const th = el("thead"); th.append(cab); t.append(th);
  const tb = el("tbody");
  ORDEM_UF.filter(([uf]) => porUf[uf]).forEach(([uf, rg], i, arr) => {
    const e = porUf[uf];
    const fraca = e.x < 2;
    const novaReg = i === 0 || arr[i - 1][1] !== rg;
    const tr = el("tr", [fraca ? "uf-fraca" : "", novaReg && i > 0 ? "inicio-regiao" : ""].join(" ").trim() || null);
    if (novaReg) {
      const tdr = el("td", "regiao");
      tdr.rowSpan = arr.filter((q) => q[1] === rg).length;
      tdr.append(el("span", "regiao-nome", REGIOES[rg]), el("span", "regiao-sigla", rg));
      tr.append(tdr);
    }
    tr.append(el("td", "uf-sigla", uf));
    for (const b of ["PT", "PL"]) {
      const td = el("td", "col-cand");
      td.append(el("span", "uf-proj", fmt(e[b])));
      if (e.pesquisa) {
        const d = Math.round(10 * (e[b] - e.pesquisa[b])) / 10;
        td.append(el("span", "uf-dif", ` (${d > 0 ? "+" : d < 0 ? "−" : "±"}${fmt(Math.abs(d))})`));
        td.title = `projeção ${fmt(e[b])}% · pesquisa ${fmt(e.pesquisa[b])}%`;
        const bg = fraca ? null : tom(b, d);
        if (bg) td.style.background = bg;
      }
      tr.append(td);
    }
    const tdb = el("td", "col-barra");
    const barra = el("span", "barra"), cheia = el("span", "barra-cheia");
    cheia.style.width = `${Math.max(0, Math.min(100, e.x))}%`;
    barra.append(cheia);
    const cx = el("span", "barra-linha");
    cx.append(barra, el("span", "barra-rot", `${fmt(e.x, 0)}%`));
    tdb.append(cx);
    tr.append(tdb);
    tb.append(tr);
  });
  t.append(tb);
}

// ---------------------------------------------------------------------------------------------------------
function iniciar() {
  const q = new URLSearchParams(location.search);
  if (q.has("replay")) {
    const passo = 1000 * (Number(q.get("replay")) || 0.6);
    carregar().then(() => {
      let i = 1; pontosVisiveis = 1;
      const tick = () => { desenhar(dados.pontos.slice(0, i)); if (i < dados.pontos.length) { i++; setTimeout(tick, passo); } };
      tick();
    });
  } else {
    carregar();
    setInterval(carregar, INTERVALO_MS);
  }
  let tmr;
  window.addEventListener("resize", () => { clearTimeout(tmr); tmr = setTimeout(() => dados && desenhar(pontosVisiveis === null ? dados.pontos : dados.pontos), 150); });
  matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => dados && desenhar(dados.pontos));
}
document.addEventListener("DOMContentLoaded", iniciar);
