  $ NO_COLOR="1" print . Docs
  <div>"lorem" & ""</div>
  

  $ pincfmt ./data.pi
  page Docs() {
    <>
      <Sub
        attributes={
          {
            bar: "lorem",
            foo?: if (false) "",
          }
        }
      />
    </>
  }
  
  component Sub() {
    let attributes = #Record(
      of: {
        bar: #String,
        lorem?: #String(key: "foo"),
      },
    );
  
    <div>"{attributes.bar}" & "{attributes.lorem}"</div>
  }
