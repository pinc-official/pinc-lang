  $ NO_COLOR="1" print . StoreValueProvider
  <div>
  Somewhere in the World ... or am I??some@email.invalid+1 111 000 111Imprint/imprint
  
  42008Product A22130Product B
  </div>

  $ pincfmt ./stores.pi
  store Settings(single: true) {
    footerLinks: #Array(
      of: #Record(
        of: {
          title: #String,
          url: #String,
        },
      ),
    ),
    contact: #Record(
      of: {
        phone: #String,
        email: #String,
        address: #String :: fn (v) -> v ++ " ... or am I??",
      },
    ),
  }
  
  store Products {
    title: #String,
    code: #Int :: fn (v) -> v * 2,
  }
  

  $ pincfmt ./StoreValueProvider.pi
  component StoreValueProvider {
    let settings = {
      footerLinks: [
        {
          title: "Imprint",
          url: "/imprint",
        },
      ],
      contact: {
        phone: "+1 111 000 111",
        email: "some@email.invalid",
        address: "Somewhere in the World",
      },
    };
  
    let products = [
      {
        title: "Product A",
        code: 21004,
      },
      {
        title: "Product B",
        code: 11065,
      },
    ];
  
    <StoreConsumer settings={settings} products={products} />
  }
  
  component StoreConsumer {
    let settings = #Store(id: Settings);
    let products = #Store(id: Products);
  
    <div>
      {settings}
  
      {products}
    </div>
  }
  
